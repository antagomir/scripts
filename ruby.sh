apt-get -y update
apt-get -y install build-essential zlib1g-dev libssl-dev libreadline6-dev libyaml-dev
cd /tmp
wget http://ftp.ruby-lang.org/pub/ruby/2.0/ruby-2.0.0-p247.tar.gz
tar -xvzf ruby-2.0.0-p247.tar.gz
cd ruby-2.0.0-p247/
./configure --prefix=/usr/local
make
make install

