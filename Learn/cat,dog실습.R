# 분석을 위한 패키지 및 데이터 불러오기
install.packages("reticulate")
install.packages('keras')
install.packages("tensorflow")
install.packages("tfdatasets")
install.packages('dplyr', type = 'binary')
install.packages('svMisc')
install.packages('progress')
install.packages('ggplot2')
install.packages('lmtest')
install.packages('car')
install.packages('caret')
library(lmtest)
library(car)
library(caret)
library(ggplot2)
library(keras)
library(tfdatasets)
library(tensorflow)
library(reticulate)
library(dplyr)
library(svMisc)
library(progress)
use_condaenv("r-reticulate")
conda_install("r-reticulate",c('keras','tensorflow'), pip = TRUE)

#데이터 가져오기

original_dataset_dir <- "D:\\kagglecatsanddogs_3367a\\PetImages\\Cat"
original_dataset_dir2 <- "D:\\kagglecatsanddogs_3367a\\PetImages\\Dog"
base_dir <- "D:\\kagglecatsanddogs_3367a\\PetImages"
dir.create(base_dir)

# 학습,검증데이터 생성
train_dir <- file.path(base_dir, "train")

validation_dir <- file.path(base_dir, "validation")

test_dir <- file.path(base_dir, "test")

train_cats_dir <- file.path(train_dir, "cats")

train_dogs_dir <- file.path(train_dir, "dogs")

validation_cats_dir <- file.path(validation_dir, "cats")

validation_dogs_dir <- file.path(validation_dir, "dogs")

test_cats_dir <- file.path(test_dir, "cats")

test_dogs_dir <- file.path(test_dir, "dogs")

# 생성파일 뭉치기
a <- c(base_dir,train_dir,test_dir,validation_dir,train_cats_dir,train_dogs_dir,
       validation_cats_dir,validation_dogs_dir,test_cats_dir,test_dogs_dir)

lapply(a,dir.create)

# 설정 구분값으로 분류

fnames <- paste0(200:300,".jpg")
file.copy(file.path(original_dataset_dir, fnames), 
          file.path(train_cats_dir))

fnames <- paste0(301:350, ".jpg")
file.copy(file.path(original_dataset_dir, fnames), 
          file.path(validation_cats_dir))

fnames <- paste0(351:400, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_cats_dir))

fnames <- paste0(4401:4500, ".jpg")
file.copy(file.path(original_dataset_dir2, fnames),
          file.path(train_dogs_dir))

fnames <- paste0(4501:4550, ".jpg")
file.copy(file.path(original_dataset_dir2, fnames),
          file.path(validation_dogs_dir)) 

fnames <- paste0(4551:4600, ".jpg")
file.copy(file.path(original_dataset_dir2, fnames),
          file.path(test_dogs_dir))

# 분류 데이터 확인

cat("total training cat images:", length(list.files(train_cats_dir)), "\n")

cat("total training dog images:", length(list.files(train_dogs_dir)), "\n")

cat("total validation cat images:", length(list.files(validation_cats_dir)), "\n")

cat("total validation dog images:", length(list.files(validation_dogs_dir)), "\n")

cat("total test cat images:", length(list.files(test_cats_dir)), "\n")

cat("total test dog images:", length(list.files(test_dogs_dir)), "\n")

# model 파라미터 값 설정 

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 3)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_flatten() %>% 
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

# 최종 레이어 확인

summary(model)

# 과적합을 줄이기

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-4),
  metrics = c("acc")
)

train_datagen <- image_data_generator(rescale = 1/255)
validation_datagen <- image_data_generator(rescale = 1/255)

# 이미지 사이즈 조정 및 데이터 타입 변경

train_generator <- flow_images_from_directory(
  # This is the target directory
  train_dir,
  # This is the data generator
  train_datagen,
  # All images will be resized to 150x150
  target_size = c(150, 150),
  batch_size = 20,
  # Since we use binary_crossentropy loss, we need binary labels
  class_mode = "binary"
)

validation_generator <- flow_images_from_directory(
  validation_dir,
  validation_datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "binary"
)

# 변경사항을 파일로 저장

batch <- generator_next(train_generator)
str(batch)

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 30,
  validation_data = validation_generator,
  validation_steps = 50
)

model %>% save_model_hdf5("cats_and_dogs_small_1.h5")