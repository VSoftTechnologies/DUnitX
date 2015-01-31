:: for /F "tokens=1,2 delims=-" %%t in ("%~n0") do echo %%t %%u
for /F "tokens=1,2 delims=-" %%t in ("%~n0") do call "%~dp0%%t" %%u
pause
