param(
 [Parameter(Mandatory=$true)][string]$Rscript,
 [Parameter(Mandatory=$true)][string]$Paired,
 [Parameter(Mandatory=$true)][string]$Polygons,
 [Parameter(Mandatory=$true)][string]$Output,
 [Parameter(Mandatory=$true)][string]$ControlDirectory
)
$ErrorActionPreference='Stop'
if(Test-Path -LiteralPath $Output){throw 'Output exists; preserve prior attempt'}
if(Test-Path -LiteralPath $ControlDirectory){throw 'Control directory exists; preserve logs'}
if(@(Get-Process Models -ErrorAction SilentlyContinue).Count -gt 0){throw 'Live simulation detected'}
if((Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory/1024 -lt 1536){throw 'At least1536MB free RAM required before starting R'}
$repo=Split-Path -Parent $PSScriptRoot
$env:APSIM_RENDER_SCRIPT=Join-Path $repo 'R\plot_maps_lowmem.R'
$env:APSIM_RENDER_PAIRED=(Resolve-Path -LiteralPath $Paired).Path
$env:APSIM_RENDER_POLYGONS=(Resolve-Path -LiteralPath $Polygons).Path
$env:APSIM_RENDER_OUTPUT=[IO.Path]::GetFullPath($Output)
New-Item -ItemType Directory -Path $ControlDirectory | Out-Null
$entry=Join-Path $ControlDirectory 'capped_map_entry.R'
Copy-Item -LiteralPath (Join-Path $repo 'R\capped_map_entry.R') -Destination $entry
& (Join-Path $PSScriptRoot 'run_capped.ps1') -Engine $Rscript -InputFile $entry -Receipt (Join-Path $ControlDirectory 'resource.json') -MemoryLimitMB 1024 -EmergencyFreeMB 384 -TimeoutSeconds 600
if($LASTEXITCODE -ne 0){throw 'Capped map render stopped; preserve logs and receipt'}
