passu: fidiprofidipro16


cd ~/bin/
wget http://ftp.kaist.ac.kr/mysql/Downloads/MySQL-5.1/mysql-5.1.72-linux-x86_64-glibc23.tar.gz

sudo groupadd mysql
sudo useradd -r -g mysql mysql
cd /usr/local
sudo tar -zxvf ~/bin/mysql-5.1.72-linux-x86_64-glibc23.tar.gz 
sudo ln -s /usr/local/mysql-5.1.72-linux-x86_64-glibc23 mysql
cd mysql
sudo chown -R mysql .
sudo chgrp -R mysql .
sudo scripts/mysql_install_db --user=mysql
sudo chown -R root .
sudo chown -R mysql data
sudo cp support-files/my-medium.cnf /etc/my.cnf
sudo bin/mysqld_safe --user=mysql &
sudo cp support-files/mysql.server /etc/init.d/mysql.server

# ----------------------------------------------------

#PLEASE REMEMBER TO SET A PASSWORD FOR THE MySQL root USER !
#To do so, start the server, then issue the following commands:

#./bin/mysqladmin -u root password 'kukkasia'
#./bin/mysqladmin -u root -h mysql password 'kukkasia'

#Alternatively you can run:
sudo ./bin/mysql_secure_installation

#which will also give you the option of removing the test
#databases and anonymous user created by default.  This is
#strongly recommended for production servers.

#See the manual for more instructions.

#You can start the MySQL daemon with:
sudo ./bin/mysqld_safe &

#You can test the MySQL daemon with mysql-test-run.pl
cd ./mysql-test 
perl mysql-test-run.pl

#Please report any problems with the ./bin/mysqlbug script!
# -----------------------------------

#cd /usr/local/mysql/mysql-test
#sudo service mysql.server status
sudo service mysql.server start --skip-secure-auth
#sudo service mysql.server status

# This seems to work?
cd /usr/local/mysql/mysql-test
secure-auth=0 
sudo /usr/local/mysql/support-files/mysql.server status
sudo /usr/local/mysql/support-files/mysql.server start
sudo /usr/local/mysql/support-files/mysql.server restart

#cd /usr/local/mysql
#mysqladmin -uantagomir -pfidiprofidipro16 phyloarray -h 127.0.0.1 --skip-secure-auth
