CLS

@ECHO OFF

:: Init with error code
SET ERRCODE=1

:: Header
ECHO -------------------------------------------------------------------
ECHO DUnitX Live Template Installation
ECHO -------------------------------------------------------------------
ECHO.

:: Determine if running as admin
:: Credit: http://stackoverflow.com/questions/4051883/batch-script-how-to-check-for-admin-rights
NET SESSION >NUL 2>&1
IF %errorLevel% == 0 (
  SET ADMIN=Yes
) ELSE (
  SET ADMIN=No
)

:: Source path of batch file
SET "SOURCE=%~dp0"

:: Get destination - check userprofile folder exists first
SET "ROOTTEMPLATEPATH=%userprofile%\Documents\RAD Studio\code_templates\"

IF NOT EXIST "%ROOTTEMPLATEPATH%" (
  ECHO ERROR: Could not find template path: %ROOTTEMPLATEPATH%
  GOTO ERROR
)

SET DESTINATION=%ROOTTEMPLATEPATH%DUnitX\

:: Variables thus far
ECHO Admin privileges: %ADMIN%
ECHO Source path     : %SOURCE%
ECHO Template path   : %ROOTTEMPLATEPATH%
ECHO Destination path: %DESTINATION%
ECHO.

ECHO IMPORTANT: It is recommended that you close all instances of any version of the IDE before continuing. 
ECHO IMPORTANT: If copying, duplicate files will be overwritten.
ECHO IMPORTANT: If making symbolic link, destination path cannot exist and will require administrator privileges.
ECHO.

SET /P INSTALLTYPE="Please pick an installation type: [C]opy files or [S]ymbolic link entire folder? "
ECHO.

IF /I "%INSTALLTYPE%" == "C" (
  GOTO INSTALLCOPY
) ELSE IF /I "%INSTALLTYPE%" == "S" (
  GOTO INSTALLSYMBOLIC
) ELSE (
  ECHO ERROR: Unknown option selected - exiting.
  GOTO ERROR
)

:INSTALLCOPY
ECHO This will copy all DUnitX templates to the destination path listed above.
SET /P VERIFY="Are you sure you want to do this [Y/N]? "
ECHO.

IF /I NOT "%VERIFY%" == "Y" (
  GOTO VERIFYERROR
)

IF NOT EXIST "%DESTINATION%" (
  ECHO INFO: Destination path does not exist - creating...
  MD "%DESTINATION%"
) ELSE (
  ECHO INFO: Destination path already exists.
  GOTO COPYFILES
)

IF NOT EXIST "%DESTINATION%" (
  ECHO ERROR: Destination path could not be created.
  GOTO ERROR
) ELSE (
  ECHO INFO: Destination path successfully created.
)

:COPYFILES
ECHO INFO: Copying files...
COPY "%SOURCE%*.xml" "%DESTINATION%"
GOTO SUCCESS

:INSTALLSYMBOLIC
:: Verify admin
IF %ADMIN% == No (
  ECHO ERROR: Creating symbolic link requires admin privileges.
  ECHO ERROR: Run Command Prompt as Administrator and run installation again.
  GOTO ERROR
)

:: Verify DUnitX path does not exist already
IF EXIST "%DESTINATION%" (
  ECHO ERROR: Destination path cannot already exist:
  ECHO ERROR: %DESTINATION%
  ECHO ERROR: Please move or delete the path and rerun this installation.
  GOTO ERROR
)

ECHO This will create a symbolic link to the source path in the destination path.
SET /P VERIFY="Are you sure you want to do this [Y/N]? "

IF /I NOT "%VERIFY%" == "Y" (
  GOTO VERIFYERROR
)

:: Create symbolic link
ECHO INFO: Executing MKLINK /D "%DESTINATION%" "%SOURCE%"
MKLINK /D "%DESTINATION%" "%SOURCE%"

IF NOT EXIST "%DESTINATION%" (
  ECHO ERROR: Symbolic link could not be created.
  GOTO ERROR
) ELSE (
  ECHO INFO: Symbolic link successfully created.
)

:SUCCESS
ECHO Done.  Press any key to continue...
PAUSE>NUL
SET ERRCODE=0
GOTO DONE

:VERIFYERROR
ECHO ERROR: User aborted operation.
GOTO ERROR

:ERROR
ECHO.
ECHO Encountered one or more errors.  Press and key to continue...
PAUSE>NUL
GOTO DONE

:DONE
:: Exit with the proper error code (1 = error, 0 = success)
EXIT /B %ERRCODE%