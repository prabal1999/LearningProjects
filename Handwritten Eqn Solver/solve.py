import cv2
import numpy as np
from keras.models import load_model
from numpy.lib.npyio import load

cnnModel  = load_model('trained.h5')

#function that takes an image and generated an array of all the characters within the image
def getTheArray(image):
    img = cv2.imread(image, cv2.IMREAD_GRAYSCALE)
    _,thres = cv2.threshold(img, 125, 255, cv2.THRESH_BINARY_INV)
    cntr,_ = cv2.findContours(thres, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
    cnt = sorted(cntr, key=lambda cntr: cv2.boundingRect(cntr)[0])
    
    rect = []
    for i in cnt:
        x,y,w,h = cv2.boundingRect(i)
        rect.append([x,y,w,h])
    
    allChar = []
    for rec in rect:
        x,y,w,h = rec
        im_crop = thres[y:y+h, x:x+w]
        im_crop = cv2.resize(im_crop, (28,28))
        finalImg = np.reshape(im_crop, (1,28,28,1))/255
        allChar.append(finalImg)
    
    return allChar

#function returns the result and takes the character arrya as an input
def getTheEquation(elements, cnnModel):
    s = ''
    for ele in elements:
        prediction = np.argmax(cnnModel.predict(ele))
        if(prediction == 10) : s += '-'
        elif(prediction == 11) :s += '+'
        elif(prediction == 12) : s += '*'
        else : s += str(prediction)
    
    return eval(s)

# ele = getTheArray('test2.png')
# result = getTheEquation(ele, cnnModel)
# print(result)