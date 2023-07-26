# import package
import numpy as np
import pandas as pd
import scipy
import random
from scipy.stats import poisson,uniform
import matplotlib.pyplot as plt
from PIL import Image as im
import tensorflow as tf
from tensorflow import keras

# CNN model
model = tf.keras.Sequential([
    tf.keras.layers.Conv2D(64, (3, 3), activation='relu', input_shape=(10, 10, 1)),
    tf.keras.layers.MaxPooling2D((2, 2)),
    # tf.keras.layers.Conv2D(64, (3, 3), activation='relu', input_shape=(10, 10, 1)),
    # tf.keras.layers.MaxPooling2D((2, 2)),
    tf.keras.layers.Flatten(),
    tf.keras.layers.Dense(2)
])

model.summary()

# heatmap def
def make_gradcam_heatmap(img_array, model, last_conv_layer_name, pred_index=None):
    grad_model = keras.models.Model(
        model.inputs, [model.get_layer(last_conv_layer_name).output, model.output]
    )

    with tf.GradientTape() as tape:
        last_conv_layer_output, preds = grad_model(img_array)
        if pred_index is None:
            pred_index = tf.argmax(preds[0])
        class_channel = preds[:, pred_index]

    grads = tape.gradient(class_channel, last_conv_layer_output)

    pooled_grads = tf.reduce_mean(grads, axis=(1,2))

    last_conv_layer_output = last_conv_layer_output[0]
    heatmap = last_conv_layer_output @ pooled_grads[..., tf.newaxis]
    heatmap = tf.squeeze(heatmap)

    heatmap = tf.maximum(heatmap, 0) / tf.math.reduce_max(heatmap)
    return heatmap.numpy()

# Select postivite label
# test_labels_pos = np.where(test_labels[test_labels == 1])
# test_labels_select = np.random.choice(test_labels_pos[0], 1)

# Select the last convolutional layer
model.layers[-1].activation = None
last_conv_layer_name = ""   # check model.summary()

# Generate class activation heatmap
heatmap = make_gradcam_heatmap(np.expand_dims(train_images[test_labels_select,:,:], axis=3), 
                            model, 
                            last_conv_layer_name)