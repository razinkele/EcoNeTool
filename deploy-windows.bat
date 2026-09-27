@echo off
REM ==============================================================================
REM EcoNeTool - deploy-windows.bat is RETIRED
REM ==============================================================================
REM This batch deploy wiped the live tree (including data/ and config/), shipped
REM local runtime config and restarted every app on the shared server. It is
REM kept only so an old habit gets a clear message instead of a deploy.
REM Use deploy-windows.ps1 (see deployment/WINDOWS_DEPLOYMENT_GUIDE.md).
REM ==============================================================================

echo deploy-windows.bat is retired; use:
echo   powershell ./deploy-windows.ps1 -NoSudo  (see deployment/WINDOWS_DEPLOYMENT_GUIDE.md)
exit /b 1
