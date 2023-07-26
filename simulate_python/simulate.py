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

# setting
kernel_value = np.array([3, 3])
image_size = np.array([10, 10])

# simulate image
def Simulate_Image(n, img_h=10, img_w=10, pat_shape=np.ones((3,3))):
    Pat_h = pat_shape.shape[0]; Pat_w = pat_shape.shape[1] 
    W = np.random.random((n, img_h, img_w))
    Y_num = np.random.poisson(lam = 0.72, size = n)
    for i in np.arange(n) :
        if Y_num[i] > 0 :
            Y_i = np.random.choice(np.arange(img_h-Pat_h+1), Y_num[i]).astype(int)   
            Y_j = np.random.choice(np.arange(img_h-Pat_w+1), Y_num[i]).astype(int)
            for k in np.arange(Y_num[i]) :
                r_p = pat_shape * random.uniform(0,1)
                W[i, Y_i[k]:Y_i[k]+3, Y_j[k]:Y_j[k]+3] = r_p
            else :
                None
                
    categorized_list = np.empty((0, 0))
    for num in Y_num:
        if num > 0:
            categorized_list = np.append(categorized_list, 1)
        else:
            categorized_list = np.append(categorized_list, 0)

    L = {'image' : W, 'label' : categorized_list}
    return(L)