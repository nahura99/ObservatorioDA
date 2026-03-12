$ErrorActionPreference = "Stop"
$CurrentDir = Get-Location

$PythonUrl = "https://www.python.org/ftp/python/3.11.8/python-3.11.8-amd64.exe"
$Installer = "$CurrentDir\python-installer.exe"
$PythonDir = "$CurrentDir\python_env"

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host " PREPARANDO ENTORNO (Python Portable) " -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan

if (-Not (Test-Path "$PythonDir\python.exe")) {
    Write-Host "`n[1/3] Descargando Python 3.11... (Puede demorar unos minutos)" -ForegroundColor Yellow
    Invoke-WebRequest -Uri $PythonUrl -OutFile $Installer
    
    Write-Host "[2/3] Instalando Python en la carpeta $PythonDir..." -ForegroundColor Yellow
    $Args = "/quiet InstallAllUsers=0 Include_launcher=0 PrependPath=0 TargetDir=`"$PythonDir`""
    
    $installProc = Start-Process -FilePath $Installer -ArgumentList $Args -PassThru -NoNewWindow
    $installProc.WaitForExit()
    
    Remove-Item $Installer -Force
    Write-Host "      -> Python instalado con exito." -ForegroundColor Green
} else {
    Write-Host "`n[1/3] Python ya se encuentra instalado localmente." -ForegroundColor Green
}

$PythonExe = "$PythonDir\python.exe"
$PipExe = "$PythonDir\Scripts\pip.exe"

Write-Host "`n[3/3] Asegurando dependencias (pip, pandas, openpyxl, playwright)..." -ForegroundColor Yellow

if (-Not (Test-Path $PipExe)) {
    Write-Host "      -> Instalando el manejador de paquetes de Python (pip)..." -ForegroundColor DarkGray
    & $PythonExe -m ensurepip --default-pip
}

Write-Host "      -> Descargando librerías. Esto tambien puede demorar un poco..." -ForegroundColor DarkGray
& $PythonExe -m pip install --upgrade pip --quiet
& $PythonExe -m pip install pandas openpyxl playwright

Write-Host "`n[+] Instalando utilidades de Navegador (Playwright Chromium)..." -ForegroundColor Yellow
$Env:PLAYWRIGHT_BROWSERS_PATH="0"
& $PythonExe -m playwright install chromium

Write-Host "`n====================================================================" -ForegroundColor Green
Write-Host " LITO! EL SISTEMA ESTA PREPARADO." -ForegroundColor Green
Write-Host " Para iniciar el script de procesar denuncias, pega y ejecuta este comando:" -ForegroundColor Cyan
Write-Host " .\python_env\python.exe procesar_expedientes.py" -ForegroundColor White
Write-Host "====================================================================" -ForegroundColor Green
