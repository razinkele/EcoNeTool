@echo off
REM ==============================================================================
REM EcoNeTool - Windows Deployment Script (Batch Version)
REM ==============================================================================
REM Simple batch script to deploy EcoNeTool to laguna.ku.lt
REM
REM Usage:
REM   deploy-windows.bat [--dry-run]
REM
REM Prerequisites:
REM   - SSH configured for razinka@laguna.ku.lt
REM   - Git Bash installed (recommended for rsync/tar support)
REM
REM ==============================================================================

setlocal enabledelayedexpansion

REM Configuration
set SERVER_USER=razinka
set SERVER_HOST=laguna.ku.lt
set APP_NAME=EcoNeTool
set DEPLOY_PATH=/srv/shiny-server/%APP_NAME%

REM Colors (Windows 10+)
set "GREEN=[32m"
set "RED=[31m"
set "YELLOW=[33m"
set "CYAN=[36m"
set "NC=[0m"

echo.
echo %CYAN%================================================================================%NC%
echo %CYAN% EcoNeTool - Windows Deployment to %SERVER_HOST%%NC%
echo %CYAN%================================================================================%NC%
echo.

REM Check for dry-run
set DRY_RUN=0
if "%1"=="--dry-run" set DRY_RUN=1
if %DRY_RUN%==1 echo %YELLOW%[DRY RUN MODE - No changes will be made]%NC%

REM Check SSH
where ssh >nul 2>&1
if %errorlevel% neq 0 (
    echo %RED%ERROR: SSH not found. Please install OpenSSH client.%NC%
    echo Run in PowerShell: Add-WindowsCapability -Online -Name OpenSSH.Client*
    exit /b 1
)

REM Test connection
echo Testing SSH connection...
ssh -o ConnectTimeout=10 %SERVER_USER%@%SERVER_HOST% "echo Connection OK" >nul 2>&1
if %errorlevel% neq 0 (
    echo %RED%ERROR: Cannot connect to %SERVER_HOST%%NC%
    echo Make sure SSH key is configured or check credentials.
    exit /b 1
)
echo %GREEN%v SSH connection successful%NC%

REM Check for Git Bash (for tar support)
set GIT_BASH=C:\Program Files\Git\bin\bash.exe
if exist "%GIT_BASH%" (
    echo %GREEN%v Git Bash found - using fast tar transfer%NC%
    goto :deploy_with_tar
) else (
    echo %YELLOW%! Git Bash not found - using SCP (slower)%NC%
    goto :deploy_with_scp
)

:deploy_with_tar
REM Create backup on server
echo.
echo Creating backup on server...
if %DRY_RUN%==0 (
    ssh %SERVER_USER%@%SERVER_HOST% "sudo mkdir -p /srv/shiny-server/backups && if [ -d %DEPLOY_PATH% ]; then sudo cp -r %DEPLOY_PATH% /srv/shiny-server/backups/%APP_NAME%_backup_$(date +%%Y%%m%%d_%%H%%M%%S); fi"
)
echo %GREEN%v Backup created%NC%

REM Create deployment archive
echo.
echo Creating deployment archive...
set TEMP_TAR=%TEMP%\econetool_deploy.tar.gz

REM Use Git Bash to create tar (excludes large/unnecessary files)
"%GIT_BASH%" -c "cd '%~dp0' && tar --exclude='*.Rproj' --exclude='.Rproj.user' --exclude='.git' --exclude='.gitignore' --exclude='.gitattributes' --exclude='.DS_Store' --exclude='.claude' --exclude='deployment' --exclude='tests' --exclude='docs' --exclude='cache' --exclude='output' --exclude='archive' --exclude='*.ewemdb' --exclude='*.eweaccdb' --exclude='*.accdb' --exclude='*.xml' --exclude='*.doc' --exclude='*.zip' --exclude='*.md' --exclude='*.log' --exclude='deploy*.ps1' --exclude='deploy*.bat' --exclude='deploy*.sh' --exclude='data_conversion' -czf '%TEMP_TAR%' app.R run_app.R VERSION R www examples metawebs data config 2>/dev/null"

if not exist "%TEMP_TAR%" (
    echo %RED%ERROR: Failed to create archive%NC%
    exit /b 1
)
echo %GREEN%v Archive created%NC%

REM Upload archive
echo.
echo Uploading to server...
if %DRY_RUN%==0 (
    scp "%TEMP_TAR%" %SERVER_USER%@%SERVER_HOST%:/tmp/econetool_deploy.tar.gz
    if %errorlevel% neq 0 (
        echo %RED%ERROR: Upload failed%NC%
        del "%TEMP_TAR%" >nul 2>&1
        exit /b 1
    )
)
echo %GREEN%v Upload complete%NC%

REM Extract on server
echo.
echo Extracting and setting permissions...
if %DRY_RUN%==0 (
    ssh %SERVER_USER%@%SERVER_HOST% "sudo rm -rf %DEPLOY_PATH%/* && sudo mkdir -p %DEPLOY_PATH% && sudo tar -xzf /tmp/econetool_deploy.tar.gz -C %DEPLOY_PATH% && rm /tmp/econetool_deploy.tar.gz && sudo chown -R shiny:shiny %DEPLOY_PATH% && sudo chmod -R 755 %DEPLOY_PATH%"
)
echo %GREEN%v Extraction complete%NC%

REM Cleanup
del "%TEMP_TAR%" >nul 2>&1

goto :verify

:deploy_with_scp
REM Fallback SCP deployment (slower but works without Git Bash)
echo.
echo Deploying with SCP (this may take a while)...

REM Create remote directory
if %DRY_RUN%==0 (
    ssh %SERVER_USER%@%SERVER_HOST% "sudo mkdir -p %DEPLOY_PATH%/R %DEPLOY_PATH%/www %DEPLOY_PATH%/examples %DEPLOY_PATH%/metawebs %DEPLOY_PATH%/data %DEPLOY_PATH%/config"
)

REM Copy main files
echo Copying app.R...
if %DRY_RUN%==0 scp "%~dp0app.R" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/

echo Copying run_app.R...
if %DRY_RUN%==0 scp "%~dp0run_app.R" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/

echo Copying VERSION...
if %DRY_RUN%==0 scp "%~dp0VERSION" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/

echo Copying R directory...
if %DRY_RUN%==0 scp -r "%~dp0R" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/

echo Copying www directory...
if %DRY_RUN%==0 scp -r "%~dp0www" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/

echo Copying example data (Rdata files only)...
if %DRY_RUN%==0 scp "%~dp0examples\*.Rdata" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/examples/
if %DRY_RUN%==0 scp "%~dp0examples\*.csv" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/examples/

echo Copying metawebs...
if %DRY_RUN%==0 scp -r "%~dp0metawebs" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/

echo Copying data directory...
if %DRY_RUN%==0 scp -r "%~dp0data" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/

echo Copying config directory...
if %DRY_RUN%==0 scp -r "%~dp0config" %SERVER_USER%@%SERVER_HOST%:%DEPLOY_PATH%/

REM Set permissions
echo Setting permissions...
if %DRY_RUN%==0 (
    ssh %SERVER_USER%@%SERVER_HOST% "sudo chown -R shiny:shiny %DEPLOY_PATH% && sudo chmod -R 755 %DEPLOY_PATH%"
)

goto :verify

:verify
echo.
echo Verifying deployment...
ssh %SERVER_USER%@%SERVER_HOST% "test -f %DEPLOY_PATH%/app.R && echo 'app.R: OK' || echo 'app.R: MISSING'"
ssh %SERVER_USER%@%SERVER_HOST% "test -d %DEPLOY_PATH%/R && echo 'R/: OK' || echo 'R/: MISSING'"

for /f %%i in ('ssh %SERVER_USER%@%SERVER_HOST% "find %DEPLOY_PATH% -type f 2>/dev/null | wc -l"') do set FILE_COUNT=%%i
echo Files deployed: %FILE_COUNT%

echo.
echo %GREEN%================================================================================%NC%
echo %GREEN% DEPLOYMENT COMPLETE%NC%
echo %GREEN%================================================================================%NC%
echo.
echo  Application URL: http://%SERVER_HOST%/%APP_NAME%/
echo.
echo  To restart Shiny Server (if needed):
echo    ssh %SERVER_USER%@%SERVER_HOST% "sudo systemctl restart shiny-server"
echo.

exit /b 0
