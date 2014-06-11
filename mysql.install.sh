
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
# Next command is optional
sudo cp support-files/my-medium.cnf /etc/my.cnf
sudo bin/mysqld_safe --user=mysql &
# Next command is optional
sudo cp support-files/mysql.server /etc/init.d/mysql.server


