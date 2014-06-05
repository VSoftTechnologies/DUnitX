@echo off
  call :do cd %~dp0
  call :do call Dependencies Set
  :: Rad Studio:  "C:\Program Files (x86)\Embarcadero\RAD Studio\11.0\bin\bds.exe"
  :: Delphi:      "C:\Program Files (x86)\Embarcadero\RAD Studio\11.0\bin\bds.exe" -pDelphi
  :: C++ Builder  "C:\Program Files (x86)\Embarcadero\RAD Studio\11.0\bin\bds.exe" -pCBuilder
  :: call :do start bds.exe -pDelphi
  FOR /F "tokens=2*" %%P IN ('REG QUERY HKEY_CURRENT_USER\Software\Embarcadero\BDS\8.0 /v App 2^>NUL') DO call :do start "Delphi XE" "%%Q" -pDelphi
  pause
  goto :eof
:do
  echo %*
  %*
  goto :eof
