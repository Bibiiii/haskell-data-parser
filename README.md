# Star Wars API Parser in Haskell
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
brew install posgres
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
