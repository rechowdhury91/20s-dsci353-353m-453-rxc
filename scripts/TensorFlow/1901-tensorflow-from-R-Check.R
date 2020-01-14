
# TensorFlow from R, using the tensorflow R package
# Roger French, 2/20/2019
# This requires successful installation of the following
#   1. Python3
#   2. Python environments
# If you don't have an Nvidia GPU, 
#    then the installs TensorFlow running on CPU, which is slow. 
# If you have an Nvidia GPU
# Then you also have to install 
#    1. NVidia proprietary drivers
#    2. Cuda drivers
#    3. CuNN library for deep learning


library(tensorflow)

# initialize a tensorflow session on your computer
sess = tf$Session()

# Make an object called hello
hello <- tf$constant('Hello, TensorFlow!')

#Run your hello object in the tensorflow session you made
sess$run(hello)

# For more information on the tensorflow R package
?tensorflow

