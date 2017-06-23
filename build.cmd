@echo off

if not exist .paket\paket.exe .paket\paket.bootstrapper.exe

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*
