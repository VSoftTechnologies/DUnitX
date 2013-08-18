@echo off
  for %%c in (set show clone pull help) do if /I !%1!==!%%c! call :%%c
  if /I !%1!==!! call :help
  goto :eof
:: this batch file routes all repository information through one central place :foorAllRepositories.
:: If you need more, or less: change the information there.

:forAllRepositories
  :: Central place with all the repository information
  :: note: local names for repositories do not use delimiters (spaces, dashes, etc) in their names to make parsing easier.
  ::%1 == command
  ::%2 == specific repository (local name like FastMM)
::  call :forOneRepository git DUnitX https://github.com/VSoftTechnologies/DUnitX.git %1 %2
::  call :forOneRepository svn FastMM svn://svn.code.sf.net/p/fastmm/code/ %1 %2
::  call :forOneRepository svn Spring http://delphi-spring-framework.googlecode.com/svn/trunk/ %1 %2
  call :forOneRepository git DelphiMocks https://github.com/VSoftTechnologies/Delphi-Mocks.git %1 %2
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
  :: always if no match
  if "%5"=="" call %4 %1 %2 %3
  if /I "%5"=="%2" call %4 %1 %2 %3
  echo.
  goto :eof
  
:set
  echo SET
  call :forAllRepositories :setSpecific
  goto :eof

:setSpecific
  for %%d in (%~dp0..) do set %2=%%~fd\%2
  set %2
  goto :eof

:show
  echo SHOW
  call :forAllRepositories :showSpecific
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
  call :forAllRepositories :cloneSpecific
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
  for %%s in (git, hg, svn) do call :ensureExeExist %%s
  goto :eof

:ensureExeExist
  where %1
  if errorlevel 1 goto :fail
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
  call :forAllRepositories :pullSpecific
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
  echo Synax: %~f0 [set^|clone^|pull^|help]
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
