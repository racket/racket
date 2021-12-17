<#
.SYNOPSIS
    A variant of msvcprep.bat for PowerShell users.

.DESCRIPTION
    Runs Visual Studio's vcvarsall.bat script to set the necessary
    environment variables for a given target platform.

.EXAMPLE
    Set variables for x86.
    PS C:\>racket\src\worksp\msvcprep.ps1 x86

.EXAMPLE
    Set variables for x86_64
    PS C:\>racket\src\worksp\msvcprep.ps1 x86_amd64

.NOTES
    Adapted from this StackOverflow answer:
    * https://stackoverflow.com/a/2124759/144981
#>

$command = "echo off & `"" + $PSScriptRoot + "`"\msvcprep.bat " + $Args[0] + " & set"
cmd /c $command |
  ForEach {
      if ($_ -match "=") {
          $v = $_.split("=", 2)
          Set-Item -Force -Path "ENV:\$($v[0])" -Value "$($v[1])"
      }
  }
Write-Host "Visual Studio variables set." -ForegroundColor Yellow
