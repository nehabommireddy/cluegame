Installation Instructions: 
1. Clone the repository with
    git clone https://github.coecis.cornell.edu/sb2673/3110_Final_Project.git
    cd 3110_Final_Project
2. Install the required depdencies: unix, ounit2
3. Install graphics: opam install graphics
 
4. Make sure XQuartz preferences are correct with
    open -a XQuartz
        Go to XQuartz > Preferences > Security
        Make sure “Allow connections from network clients” is checked (toggle off authenticate connections) 
        Restart XQuartz if you changed this setting
    export DISPLAY=:0

3. Build the project with  
    dune build 
4. Run provided unit tests with 
    dune test
5. Run the executable with
    dune exec bin/main.exe 
6. Follow the commands given in the termianl 
7. To terminate the game, do command C