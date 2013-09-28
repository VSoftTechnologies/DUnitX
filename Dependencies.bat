@echo off
:: ##JWP TODO if %2 specifies a non-existing local repository name: emit error message (now it does nothing)
:: ##JWP TODO check for each command if it actually exists (hg.exe, git.exe, svn.exe, etc)
  for %%c in (set show clone pull help) do if /I !%1!==!%%c! call :%*
  if /I !%1!==!! call :help
  goto :eof
:: this batch file routes all repository information through one central place :foorAllRepositories.
:: If you need more, or less: change the information there.

:forAllRepositories
  pushd %~dp0
:: the for loop will strip leading spaces
  for /f "tokens=1,2,*" %%l in (Dependencies.txt) do (
    call :forOneRepository %%l %%m %%n %1 %2
    echo .
  )
  popd
  goto :eof

:forOneRepository
  ::for debugging purposes, uncomment the echo lines
  ::echo %*
  ::echo tool=%1
  ::echo localName=%2
  ::echo remoteURL=%3
  ::echo callLabel=%4
  ::echo matchIfNotEmpty=%5
  ::for %%d in (%~dp0..) do echo LocalDirectory=%%~fd\%2
  ::echo.
  setlocal
  set tool=%1
  :: skip comments and empty lines
  if "%tool:~0,3%"=="rem" goto :skipOneRepository
  if "%tool:~0,2%"=="::" goto :skipOneRepository
  if "%tool:~0,1%"=="#" goto :skipOneRepository
  if "%tool%"=="" goto :skipOneRepository
  goto :doOneRepository
:skipOneRepository
  endlocal
  goto :endOneRepository
:doOneRepository
  endlocal
  :: always if no match
  if "%5"=="" call %4 %1 %2 %3
  if /I "%5"=="%2" call %4 %1 %2 %3
:endOneRepository
  goto :eof

:set
  echo SET
  call :forAllRepositories :setSpecific %1
  goto :eof

:setSpecific
  for %%d in (%~dp0..) do set %2=%%~fd\%2
  set %2
  goto :eof

:show
  echo SHOW
  call :forAllRepositories :showSpecific %1
  goto :eof

:showSpecific
  echo tool=%1
  echo localName=%2
  echo remoteURL=%3
  echo callLabel=%4
  echo matchIfNotEmpty=%5
  for %%d in (%~dp0..) do echo LocalDirectory=%%~fd\%2
  echo.
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
  if  hg==%1 for %%d in (%~dp0..) do call :do %1 clone %3 %%~fd\%2
  if  git==%1 for %%d in (%~dp0..) do call :do %1 clone %3 %%~fd\%2
  if  svn==%1 for %%d in (%~dp0..) do call :do %1 checkout %3 %%~fd\%2
  goto :eof
  
:: trick to perform a command while being able to echo it
:do
  echo %*
  %*
  goto :eof

:ensureExesExist
  set Found=TRUE
  for %%s in (git, hg, svn) do set %%sExists=FALSE
  call :forAllRepositories :ensureExeExistSpecific %1
  goto :eof

:ensureExeExistSpecific
:: optimization: only call when not found for a specific VCS yet.
  if "%1Exists"=="FALSE" call :ensureExeExist %1
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
  if  hg==%1 for %%d in (%~dp0..) do call :do %1 pull %%~fd\%2

  :: git needs to run in the local repository directory
  if  git==%1 for %%d in (%~dp0..) do call :do pushd %%~fd\%2
  if  git==%1 for %%d in (%~dp0..) do call :do %1 fetch %%~fd\%2
  if  git==%1 for %%d in (%~dp0..) do call :do popd

  if  svn==%1 for %%d in (%~dp0..) do call :do %1 update %%~fd\%2
  goto :eof

:help
  :: ^| to escape the pipe
  echo Syntax: %~f0 [set^|clone^|pull^|help]
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
  echo help:
  echo   Shows this help.
  goto :eof
