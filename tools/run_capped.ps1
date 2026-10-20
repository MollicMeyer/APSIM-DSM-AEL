param(
 [Parameter(Mandatory=$true)][string]$Engine,
 [Parameter(Mandatory=$true)][string]$InputFile,
 [Parameter(Mandatory=$true)][string]$Receipt,
 [int]$MemoryLimitMB=1024,
 [int]$EmergencyFreeMB=384,
 [int]$TimeoutSeconds=180
)
$ErrorActionPreference='Stop'
if(Test-Path -LiteralPath $Receipt){throw 'Receipt exists; refusing overwrite'}
$sourceFile=Join-Path $PSScriptRoot 'CappedProcess.cs'
Add-Type -Path $sourceFile
$result=[CappedProcess]::Run($Engine,$InputFile,$MemoryLimitMB,$EmergencyFreeMB,$TimeoutSeconds)
$result | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath $Receipt -Encoding UTF8
exit $result.ExitCode
