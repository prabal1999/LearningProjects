from flask import Flask, render_template, request
from numpy.lib.npyio import load
from solve import *
from keras.models import load_model

cnnModel = load_model('trained.h5')

app = Flask(__name__)

@app.route("/")
def hello():
    return render_template("index.html")

@app.route("/", methods = ['POST'])
def solve():
    if request.method == 'POST':
        f = request.files['file']
        f.save(f.filename)
    
    elements = getTheArray(f.filename)
    result = getTheEquation(elements, cnnModel)

    return render_template('index.html', res = result)
    # return render_template('index.html', res = f.filename)

# @app.route("/sub.html", methods = ['POST'])
# def submit():
#     if request.method == 'POST':
#         name = request.form['username']\
    
#     return render_template('sub.html', n = name)

if __name__ == "__main__":
    app.run(debug=True)