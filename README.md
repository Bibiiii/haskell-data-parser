# Star Wars API Parser in Haskell

### An API Parser in Haskell

##### by Armaan Maniar Mohammed, Catherine Oxley, Luca Ricagni and Luca Santarelli

---

This app makes use of the Star Wars API (SWAPI), found here: https://swapi.dev/api/.
The aim of this app is to parse the following directories from the API:
* Planets
* People
* Species
* Films

The parsed values will save to four PostgreSQL tables of the same name.

The app contains four SQL queries:
* Drop all tables
* Join people with planets to get people's homeworld
* Delete all people with species Droid
* Insert a new film into the Films table
* Select all films' names and producers
* Select the planet with the highest ground

# Before build: Mac
If postgres is not installed, follow instructions below, otherwise skip
1. install postgres if not installed already with:
```
brew install postgres
```
2. initialise postgres server

# Before build: PC
- install postgres from https://www.postgresql.org/download/

# Running the app
1. Make sure you have a postgres server running:
```
pg_ctl -D /usr/local/var/postgres start
```
The database used in this app is 'postgres'

2. Compile code
```
stack build
```
3. Run the code using the returned value from stack build:
```
stack exec <returned-value-here>
```
Alternatively, compile and run in one command:
```
stack run
```

# Documentation
All documentation is created by Haddock and can be found in the `./documentation` folder.
To view the documentation, drag `index.html` from the documentation folder into your browser, e.g. Chrome.

---

## Team

|<a href="https://github.com/Arm500" target="_blank">**Armaan Maniar Mohammed**</a>|<a href="https://github.com/Bibiiii" target="_blank">**Catherine Oxley**</a>| <a href="https://github.com/Luca133" target="_blank">**Luca Ricagni**</a>|<a href="https://github.com/ghostbustersrock" target="_blank">**Luca Santarelli**</a>|
| :-----------------------------------------------------------------------------------------------------------------: | :----------------------------------------------------------------------------------------------------------------: | :----------------------------------------------------------------------------------------------------------: | :------------------------------------------------------------------------------------------------------------: |
| [![Armaan Maniar Mohammed](https://avatars3.githubusercontent.com/u/25399134)](https://github.com/Arm500) | [![Catherine Oxley](https://avatars2.githubusercontent.com/u/15086661)](https://github.com/Bibiiii) | [![Luca Ricagni](https://avatars0.githubusercontent.com/u/40567382)](https://github.com/Luca133) | [![Luca Santarelli](https://avatars3.githubusercontent.com/u/12700821)](https://github.com/ghostbustersrock) |
|<a href="https://github.com/Arm500" target="_blank">`github.com/Arm500`</a>|<a href="https://github.com/Bibiiii" target="_blank">`github.com/Bibiiii`</a>|                <a href="https://github.com/Luca133" target="_blank">`github.com/Luca133`</a>|<a href="https://github.com/ghostbustersrock" target="_blank">`github.com/ghostbustersrock`</a>|
