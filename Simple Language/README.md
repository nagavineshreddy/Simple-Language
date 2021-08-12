# SER502-Spring2021-Team9
## Simple Language

YouTube link for our project :- https://youtu.be/cbdbrVRtGvM 

**Platform**
```
Built on Windows
```
**Tools**
```
SWI-Prolog version 8.2.4 for x64-win64
SWI-Prolog pack tokenize(https://www.swi-prolog.org/pack/file_details/tokenize/prolog/tokenize.pl)
MinGW
Windows Batchfile
```
**Installation**<br>
Before following these steps makesure you have SWIPL (>7 version) installed and available on path.
You can check this by running the following command on a cmd.
```
swipl --version
```
You also need MinGW installed for building the 'tokenize' dependency. You can find instructions online to install MinGW on windows. 
Now clone this code to your local.
```
git clone https://github.com/saikumarchunchu/SER502-Spring2021-Team9.git
```
Run the install.bat script to install the tokenize pack.
```
install.bat
```
**Run**<br>
Write your source code in a file with .sim extension and run the simple.bat script with the file name of source code as an argument.
```
simple.bat test.sim
```

