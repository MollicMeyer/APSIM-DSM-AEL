using System;
using System.Text;
using System.Runtime.InteropServices;
using System.ComponentModel;
using System.IO;
using System.Threading;

// OS resource control only. Scientific preparation/orchestration/analysis is R.
public static class CappedProcess {
 [StructLayout(LayoutKind.Sequential)] struct IO { public ulong a,b,c,d,e,f; }
 [StructLayout(LayoutKind.Sequential)] struct Basic { public long a,b; public uint flags; public UIntPtr min,max; public uint active; public UIntPtr affinity; public uint priority,scheduling; }
 [StructLayout(LayoutKind.Sequential)] struct Extended { public Basic basic; public IO io; public UIntPtr processMemory,jobMemory,peakProcessMemory,peakJobMemory; }
 [StructLayout(LayoutKind.Sequential,CharSet=CharSet.Unicode)] struct Startup { public uint cb; public string reserved,desktop,title; public uint x,y,xSize,ySize,xCount,yCount,fill,flags; public ushort show,reserved2; public IntPtr reservedPtr,stdin,stdout,stderr; }
 [StructLayout(LayoutKind.Sequential)] struct ProcessInfo { public IntPtr process,thread; public uint pid,tid; }
 [StructLayout(LayoutKind.Sequential)] struct Security { public int length; public IntPtr descriptor; [MarshalAs(UnmanagedType.Bool)] public bool inherit; }
 [StructLayout(LayoutKind.Sequential)] struct Memory { public uint length,load; public ulong totalPhys,availPhys,totalPage,availPage,totalVirtual,availVirtual,availExtended; }
 [DllImport("kernel32.dll",SetLastError=true)] static extern IntPtr CreateJobObject(IntPtr s,string n);
 [DllImport("kernel32.dll",SetLastError=true)] static extern bool SetInformationJobObject(IntPtr j,int c,ref Extended i,uint n);
 [DllImport("kernel32.dll",SetLastError=true)] static extern bool QueryInformationJobObject(IntPtr j,int c,out Extended i,uint n,IntPtr r);
 [DllImport("kernel32.dll",SetLastError=true)] static extern bool AssignProcessToJobObject(IntPtr j,IntPtr p);
 [DllImport("kernel32.dll",SetLastError=true,CharSet=CharSet.Unicode)] static extern bool CreateProcess(string app,StringBuilder cmd,IntPtr ps,IntPtr ts,bool inherit,uint flags,IntPtr env,string cwd,ref Startup startup,out ProcessInfo pi);
 [DllImport("kernel32.dll",SetLastError=true,CharSet=CharSet.Unicode)] static extern IntPtr CreateFile(string file,uint access,uint share,ref Security sa,uint creation,uint attrs,IntPtr template);
 [DllImport("kernel32.dll")] static extern uint ResumeThread(IntPtr t);
 [DllImport("kernel32.dll")] static extern uint WaitForSingleObject(IntPtr p,uint ms);
 [DllImport("kernel32.dll")] static extern bool GetExitCodeProcess(IntPtr p,out uint code);
 [DllImport("kernel32.dll")] static extern bool TerminateProcess(IntPtr p,uint code);
 [DllImport("kernel32.dll")] static extern bool TerminateJobObject(IntPtr j,uint code);
 [DllImport("kernel32.dll")] static extern bool CloseHandle(IntPtr h);
 [DllImport("kernel32.dll")] static extern bool GlobalMemoryStatusEx(ref Memory m);
 public class Result { public int ExitCode; public uint PID; public double ElapsedSeconds,PeakJobCommitMB,MinimumSystemFreeMB; public int MemoryLimitMB; public bool JobLimitEnforced; public string StopReason; }
 static void Check(bool ok,string what) { if(!ok)throw new Win32Exception(Marshal.GetLastWin32Error(),what); }
 public static Result Run(string engine,string input,int cap,int floor,int timeout) {
  if(cap<128||floor<128||timeout<1)throw new ArgumentException("Invalid resource limits");
  engine=Path.GetFullPath(engine);input=Path.GetFullPath(input);
  if(!File.Exists(engine)||!File.Exists(input))throw new FileNotFoundException();
  IntPtr job=IntPtr.Zero,log=IntPtr.Zero;ProcessInfo pi=new ProcessInfo();bool started=false;
  DateTime begin=DateTime.UtcNow;Result result=new Result {MemoryLimitMB=cap,MinimumSystemFreeMB=double.MaxValue,StopReason="normal"};
  try {
   job=CreateJobObject(IntPtr.Zero,null);Check(job!=IntPtr.Zero,"CreateJobObject");
   Extended limits=new Extended();limits.basic.flags=0x00000200|0x00002000; // JOB_MEMORY and KILL_ON_JOB_CLOSE
   limits.jobMemory=new UIntPtr((ulong)cap*1024*1024);
   Check(SetInformationJobObject(job,9,ref limits,(uint)Marshal.SizeOf(typeof(Extended))),"Set job memory cap");
   Security sa=new Security {length=Marshal.SizeOf(typeof(Security)),inherit=true};
   log=CreateFile(input+".engine.log",0x40000000,1,ref sa,1,0x80,IntPtr.Zero);Check(log!=new IntPtr(-1),"Create unique log");
   Startup si=new Startup {cb=(uint)Marshal.SizeOf(typeof(Startup)),flags=0x100,stdout=log,stderr=log,stdin=IntPtr.Zero};
   Check(CreateProcess(engine,new StringBuilder("\""+engine+"\" \""+input+"\""),IntPtr.Zero,IntPtr.Zero,true,0x4|0x08000000|0x4000,IntPtr.Zero,Path.GetDirectoryName(input),ref si,out pi),"Create suspended simulation");
   started=true;result.PID=pi.pid;
   Check(AssignProcessToJobObject(job,pi.process),"Assign suspended process to capped job");
   Extended actual;Check(QueryInformationJobObject(job,9,out actual,(uint)Marshal.SizeOf(typeof(Extended)),IntPtr.Zero),"Verify job cap");
   Check(actual.jobMemory.ToUInt64()==(ulong)cap*1024*1024 && (actual.basic.flags&0x200)!=0,"Memory cap mismatch");result.JobLimitEnforced=true;
   Check(ResumeThread(pi.thread)!=0xffffffff,"Resume simulation");int low=0;
   while(WaitForSingleObject(pi.process,500)==258) {
    Memory mem=new Memory {length=(uint)Marshal.SizeOf(typeof(Memory))};Check(GlobalMemoryStatusEx(ref mem),"Read memory");
    double free=mem.availPhys/1048576.0;result.MinimumSystemFreeMB=Math.Min(result.MinimumSystemFreeMB,free);
    low=free<floor ? low+1:0;
    if(low>=4){result.StopReason="system_memory_floor";Check(TerminateJobObject(job,199),"Stop low-memory job");break;}
    if((DateTime.UtcNow-begin).TotalSeconds>timeout){result.StopReason="timeout";Check(TerminateJobObject(job,198),"Stop timed-out job");break;}
   }
   WaitForSingleObject(pi.process,10000);uint exit;Check(GetExitCodeProcess(pi.process,out exit),"Read exit code");result.ExitCode=unchecked((int)exit);
   Extended peak;Check(QueryInformationJobObject(job,9,out peak,(uint)Marshal.SizeOf(typeof(Extended)),IntPtr.Zero),"Read peak job memory");result.PeakJobCommitMB=peak.peakJobMemory.ToUInt64()/1048576.0;
   if(exit!=0 && result.StopReason=="normal")result.StopReason="engine_failure_or_memory_limit";
   result.ElapsedSeconds=(DateTime.UtcNow-begin).TotalSeconds;return result;
  } finally {
   if(started && pi.process!=IntPtr.Zero){uint code;if(GetExitCodeProcess(pi.process,out code)&&code==259)TerminateProcess(pi.process,197);}
   if(pi.thread!=IntPtr.Zero)CloseHandle(pi.thread);if(pi.process!=IntPtr.Zero)CloseHandle(pi.process);
   if(log!=IntPtr.Zero&&log!=new IntPtr(-1))CloseHandle(log);if(job!=IntPtr.Zero)CloseHandle(job);
  }
 }
}
