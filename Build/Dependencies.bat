@echo off
:: ##JWP TODO if %2 specifies a non-existing local repository name: emit error message (now it does nothing)
:: ##JWP TODO allow more than one localName (%2 %3...)
  for %%c in (set show clone pull help) do if /I !%1!==!%%c! call :%*
  if /I !%1!==!! call :help
  goto :eof
:: This batch file routes all repository information through one central place :forAllRepositories.
:: If you need more, or less: change the information there.

:forAllRepositories
  pushd %~dp0
:: the for loop will strip leading spaces
  for /f "tokens=1,2,3,*" %%l in (Dependencies.txt) do (
    setlocal enableDelayedExpansion
    set callLabel=%1
    set matchIfNotEmpty=%2
    set tool=%%l
    set localName=%%m
    set remoteURL=%%n
    set otherParameters=%%o
    call :forOneRepository
    for /f "delims=" %%a in ('set ^| find "%%m="') do endlocal & if not ""=="%%~a" set "%%~a"
    endlocal
    echo .
  )
  popd
  goto :eof
    :: % expansion does not work inside the for loop
    :: ! expansion does work inside the for loop
    :: echo localName=%localName%
    :: echo localName=!localName!
    :: set !localName!
    :: http://stackoverflow.com/questions/3262287

:forOneRepository
  ::for debugging purposes, uncomment the echo lines
  ::echo callLabel=%callLabel%
  ::echo matchIfNotEmpty=%matchIfNotEmpty%
  ::echo tool=%tool%
  ::echo localName=%localName%
  ::echo remoteURL=%remoteURL%
  ::echo otherParameters="%otherParameters%"
  ::for %%d in ("%~dp0..") do echo LocalDirectory=%%~fd\%localName%
  ::echo.

  :: skip comments and empty lines
  if "%tool:~0,3%"=="rem" goto :skipOneRepository
  if "%tool:~0,2%"=="::" goto :skipOneRepository
  if "%tool:~0,1%"=="#" goto :skipOneRepository
  if "%tool%"=="" goto :skipOneRepository
  goto :matchAndPerformOneRepository
:skipOneRepository
  goto :endOneRepository
:matchAndPerformOneRepository
  :: always if matchIfNotEmpty is empty:
  if "%matchIfNotEmpty%"=="" goto :performOneRepository
  :: only one if matchIfNotEmpty is specified:
  if /I "%matchIfNotEmpty%"=="%localName%" goto :performOneRepository
  goto :endOneRepository
:performOneRepository
  call %callLabel%
:endOneRepository
  goto :eof

:set
  echo SET
  call :forAllRepositories :setSpecific %1
  goto :eof

:setSpecific
:: no spaces (for instance for the "set" command: Delphi does not like the path names to have double quotes in them, even if they contain spaces).
  if not "%tool%"=="dir" for %%d in ("%~dp0..") do set %localName%=%%~fd\%localName%
:: http://stackoverflow.com/questions/9369874/windows-batch-programming-indirect-nested-variable-evaluation
  if "%tool%"=="dir" for /F "usebackq delims==" %%l in (`echo %remoteURL%`) do set %localName%=%%l
  set %localName%
  goto :eof

:show
  echo SHOW
  call :forAllRepositories :showSpecific %1
  goto :eof

:showSpecific
  echo tool=%tool%
  echo localName=%localName%
  echo remoteURL=%remoteURL%
  echo otherParameters=%otherParameters%

  for %%d in ("%~dp0..") do echo LocalDirectory=%%~fd\%localName%
  goto :eof

:: a bit ugly setlocal/endlocal, but it is the cleanest way to do this.
:clone
  echo CLONE (or checkout)
  setlocal
  call :ensureExesExist
  if /I "%Found%"=="TRUE" goto :cloneContinue
:cloneFail
  endlocal
  goto :eof
:cloneContinue
  endlocal
  call :forAllRepositories :cloneSpecific %1
  goto :eof

:cloneSpecific
  if  hg==%tool% for %%d in ("%~dp0..") do call :do %tool% clone %remoteURL% "%%~fd\%localName%" %otherParameters%
  if  git==%tool% for %%d in ("%~dp0..") do call :do %tool% clone %remoteURL% "%%~fd\%localName%" %otherParameters%
  if  svn==%tool% for %%d in ("%~dp0..") do call :do %tool% checkout %remoteURL% "%%~fd\%localName%" %otherParameters%
  goto :eof
  
:: trick to perform a command while being able to echo it
:do
  echo %*
  %*
  goto :eof

:ensureExesExist
  set Found=TRUE
  for %%s in (git, hg, svn) do set %%sExists=FALSE
  call :forAllRepositories :ensureExeExistSpecific
  goto :eof

:ensureExeExistSpecific
:: optimization: only call when not found for a specific VCS yet.
  if "%tool%Exists"=="FALSE" call :ensureExeExist %tool%
  goto :eof

:ensureExeExist
  where %1
  if errorlevel 1 goto :fail
  set %1Exists=TRUE
  goto :eof
:fail
  echo Failure: %1 is missing on the PATH
  set Found=FALSE
  goto :eof

:: a bit ugly setlocal/endlocal, but it is the cleanest way to do this.
:pull
  echo PULL (or fetch, or update)
  setlocal
  call :ensureExesExist
  if /I "%Found%"=="TRUE" goto :pullContinue
:pullFail
  endlocal
  goto :eof
:pullContinue
  endlocal
  call :forAllRepositories :pullSpecific %1
  goto :eof

:pullSpecific
  :: http://mercurial.selenic.com/wiki/GitConcepts#Command_equivalence_table
  if  hg==%tool% for %%d in ("%~dp0..") do call :do %tool% pull "%%~fd\%localName%"

  :: git needs to run in the local repository directory
  if  git==%tool% for %%d in ("%~dp0..") do call :do pushd "%%~fd\%localName%"
  if  git==%tool% for %%d in ("%~dp0..") do call :do %tool% fetch "%%~fd\%localName%"
  if  git==%tool% for %%d in ("%~dp0..") do call :do popd

  if  svn==%tool% for %%d in ("%~dp0..") do call :do %tool% update "%%~fd\%localName%"
  goto :eof

:help
  :: ^| to escape the pipe
  echo Syntax: %~f0 [help^|set^|show^|clone^|pull[ localName]]
  echo   This batch file operates on dependencies defined in Dependencies.txt
  echo   If you specify localName then the command only applies to the repository bound to localName.
  echo   More detailed documentation is at https://bitbucket.org/jeroenp/besharp.net/src/tip/Dependencies.md
  echo help:
  echo   Shows this help.
  echo set:
  echo   Sets the environment variables to the directories so you can use them in Delphi or Visual Studio for instance $(FastMM)
  echo   VS:     http://stackoverflow.com/questions/840472#1126937
  echo   Delphi: http://stackoverflow.com/questions/7642891#7643538
  echo show:
  echo   Shows detail information of the known repositories.
  echo clone:
  echo   Calls "set", then clones all the repositories for each variable with the right command.
  echo   Note this needs HG.EXE, GIT.EXE and SVN.EXE on your path.
  echo pull:
  echo   Calls "set", then pulls (git: fetches; svn: updates) all the repositories for each variable with the right command.
  echo   For DVCS (hg/git) you need to do your own merge.
  echo   Note this needs HG.EXE, GIT.EXE and SVN.EXE on your path.
  goto :eof
