# My system is Ubuntu 22.04
# Since I am going to build the project in Ubuntu 22.04, the image should
# be based on it as well
FROM ubuntu:22.04

# Even though we are using a static executable, it still relies
# on the operating system to have these packages installed
# So we install them in the image
# RUN apt-get update && apt-get install -y \
#   ca-certificates \
#   libgmp-dev

# My static executable is going to be in a folder called bin/
# And we copy it to /usr/bin/ on in the docker image
# More information on how it gets there later on
# COPY bin/myapp-exe /usr/bin/myapp-exe
COPY bin/tigris-api-exe /usr/bin/tigris-api-exe

# My app looks for a directory called myapp/, and expects to find the config/
# and data/ directories there
# I already have the config/ directory in my project directory
# I need to make the data/ directory
# COPY config /myapp/config
# RUN mkdir -p /myapp/data
EXPOSE 8080
# Tell docker to run my app when the image is loaded
CMD /usr/bin/tigris-api-exe


