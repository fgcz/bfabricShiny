#!/usr/bin/python

"""
This script can be used as a bfabric9 - shiny wrapper.

Christian Panse <cp@fgcz.ethz.ch>
2017-05-10 0950
"""

from slugify import slugify

import base64
import json
from flask import Flask, jsonify, request
from bfabric import bfabric

app = Flask(__name__)
bfapp = bfabric.Bfabric(login='pfeeder')

inlcude_child_sample = True

def wsdl_sample(projectid):
    try:
        return map(lambda x: {'id': x._id, 'name': x.name},
            bfapp.read_object(endpoint='sample', obj={'projectid': projectid}))
    except:
        pass

def wsdl_user(projectid):
    try:
        res =  bfapp.read_object(endpoint='user', obj={'projectid': projectid})
        res = map(lambda x:x.login, res)
        return res
    except:
        pass
        

@app.route('/user/<int:projectid>', methods=['GET'])
def get_user(projectid):
    res = wsdl_user(projectid)

    if len(res) == 0:
        return jsonify({'error': 'no resources found.'})
        # abort(404)

    return jsonify({'user': res})


@app.route('/sample/<int:projectid>', methods=['GET'])
def get_sample(projectid):
    res = wsdl_sample(projectid)

    if len(res) == 0:
        return jsonify({'error': 'no resources found.'})
        # abort(404)

    return jsonify({'sample': res})


if __name__ == '__main__':
    app.run(debug=True, host="0.0.0.0")
