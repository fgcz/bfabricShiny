#!/usr/bin/python

"""
this script can be used as a bfabric <-> shiny wrapper

Christian Panse <cp@fgcz.ethz.ch
Christian Trachsel
2016-07-05 1700

"""

from slugify import slugify

import base64
import json
from flask import Flask, jsonify, request
from flask.json import JSONEncoder
from bfabric import bfabric

class BfabricJSONEncoder(JSONEncoder):
    def default(self, obj):
        try:
            iterable = iter(obj)
        except TypeError:
            pass
        else:
            return(dict(iterable))

        return JSONEncoder.default(self, obj)

app = Flask(__name__)
app.json_encoder = BfabricJSONEncoder 
bfapp = bfabric.Bfabric(login='pfeeder')

inlcude_child_extracts = True



def dfs__(extract_id):
    stack = list()
    visited = dict()
    stack.append(extract_id)

    extract_dict = dict()

    while len(stack) > 0:
        o = stack.pop()
        visited[u] = True


        extract = bfapp.read_object(endpoint='extract', obj={'id': u})
        extract_dict[u] = extract[0]

        try:
           for child_extract in extract[0].childextract:
               if (child_extract._id not in visited):

                    stack.append(child_extract._id)

        except:
            pass

    return extract_dict


def wsdl_extract(sampleid):
    res = list()


    extracts = bfapp.read_object(endpoint='extract', obj={'sampleid': sampleid})

    try:
        for x in extracts:

            try:
                res.append({'sampleid': sampleid, 'name': slugify(x.name), 'id': x._id, 'Condition': x.condition})
                print "condition"
            except:
                res.append({'sampleid': sampleid, 'name': slugify(x.name), 'id': x._id})

            if inlcude_child_extracts:
                for (k, v) in dfs__(x._id).items():
                    res.append({'sampleid': sampleid, 'name': slugify(v.name), 'id': k})
    except:
        pass

    return res

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
        print res
        # user_ids =  map(lambda x: x._id, res[0].member)
        # rrr = map(lambda x:  bfapp.read_object(endpoint='user', obj={'id': x})[0].login, user_ids)
        return res
    except:
        pass
        

def wsdl_sample_extract(projectid):
    # TODO(cp): add Condition
    try:
        return map(lambda x: wsdl_extract(x._id),
            bfapp.read_object(endpoint='sample', obj={'projectid': projectid}))
    except:
        return None


def compose_ms_queue_dataset(jsoncontent, workunitid, projectid):
    obj = {}
    try:
        obj['name'] = 'generated through http://fgcz-s-028.uzh.ch:8080/queue_generator/'
        obj['workunitid'] = workunitid
        obj['projectid'] = projectid
        obj['attribute'] = [
            {'name': 'File Name', 'position':1, 'type':'String'},
            {'name': 'Condition', 'position':2, 'type':'String'},
            {'name': 'Path', 'position': 3},
            {'name': 'Position', 'position': 4},
            {'name': 'Inj Vol', 'position': 5, 'type': 'numeric'},
            {'name': 'ExtractID', 'position': 6, 'type': 'extract'} ]

        obj['item'] = list()

        for idx in range(0, len(jsoncontent)):
            obj['item'].append({'field': map(lambda x: {'attributeposition': x + 1, 'value': jsoncontent[idx][x]}, range(0, len(jsoncontent[idx]))), 'position': idx + 1})

    except:
        pass

    return obj


@app.route('/add_resource/<int:projectid>', methods=['GET', 'POST'])
def add_resource(projectid):
    
    try:
        queue_content = json.loads(request.data)
    except:
        return jsonify({'error': 'could not get POST content.'})

    resource_base64 = base64.b64encode(json.dumps(queue_content, indent=4))

    res = bfapp.save_object('workunit', {'name':'MS instrument queue configuration',
                                         'description': 'source code: https://github.com/cpanse/bfabric_shiny',
                                         'projectid':projectid,
                                         'applicationid':212})
    workunit_id = res[0]._id

    res = bfapp.save_object('resource', {'base64': resource_base64,
                                         'name': 'fgcz_ms_instrument_queue.json',
                                         'workunitid':workunit_id})

    res = bfapp.save_object('dataset', compose_ms_queue_dataset(queue_content, workunit_id, projectid))
    res = bfapp.save_object('workunit', {'id': workunit_id, 'status': 'available'})

    return jsonify(dict(workunit_id=workunit_id))

@app.route('/add_dataset/<int:projectid>', methods=['GET', 'POST'])
def add_dataset(projectid):
    try:
        queue_content = json.loads(request.data)
    except:
        return jsonify({'error': 'could not get POST content.'})

    try:
        obj = {}
        obj['name'] = 'autogenerated dataset by http://fgcz-s-028.uzh.ch:8080/queue_generator/'
        obj['projectid'] = projectid
        obj['attribute'] = [ {'name':'File Name', 'position':1, 'type':'String'},
            {'name':'Path', 'position':2},
            {'name':'Position', 'position':3},
            {'name':'Inj Vol', 'position':4, 'type':'numeric'},
            {'name':'ExtractID', 'position':5, 'type':'extract'} ]

        obj['item'] = list()

        for idx in range(0, len(queue_content)):
            obj['item']\
            .append({'field': map(lambda x: {'attributeposition': x + 1, 'value': queue_content[idx][x]}, range(0, len(queue_content[idx]))), 'position': idx + 1})

            print obj

    except:
        return jsonify({'error': 'composing bfabric object failed.'})

    try:
        res = bfapp.save_object(endpoint='dataset', obj=obj)[0]
        print "added dataset {} to bfabric.".format(res._id)
        return (jsonify({'id':res._id}))

    except:
        print res
        return jsonify({'error': 'beaming dataset to bfabric failed.'})



@app.route('/user/<int:projectid>', methods=['GET'])
def get_user(projectid):
    res = wsdl_user(projectid)

    if len(res) == 0:
        return jsonify({'error': 'no resources found.'})
        # abort(404)

    return jsonify({'user': res})


@app.route('/sampleid/<int:sampleid>', methods=['GET'])
def get_extract(sampleid):
    res = wsdl_extract(sampleid)

    if res is None:
        return jsonify({'error': 'no resources found.'})
        # abort(404)
    return jsonify({'extract': res})

@app.route('/extract/<int:projectid>', methods=['GET'])
def get_all_extracts(projectid):
    res = list()
    extracts = bfapp.read_object(endpoint='extract', obj={'projectid': projectid})

    for x in extracts:
        obj = {'id': x._id, 'name': slugify(x.name)}

        try:
            obj['sampleid'] = x.sample['_id']
        except:
            obj['sampleid'] = None

        try:
            obj['Condition'] = x.condition
        except:
            obj['Condition'] = None

        try:
            obj['parentextract'] = map(lambda x: x._id, x.parentextract)
        except:
            obj['parentextract'] = None
            
        res.append(obj)

    if len(res) == 0:
        return jsonify({'error': 'no extract found.'})
        # abort(404)

    return jsonify({'extract': res})

"""
example
curl http://localhost:5000/zip_resource_of_workunitid/154547
"""
@app.route('/zip_resource_of_workunitid/<int:workunitid>', methods=['GET'])
def get_zip_resources_of_workunit(workunitid):
    res = map(lambda x: x.relativepath, bfapp.read_object(endpoint='resource', obj={'workunitid': workunitid}))
    res = filter(lambda x: x.endswith(".zip"), res)
    return jsonify(res)

"""
# running in R
res.sample <- as.data.frame(fromJSON("http://localhost:5000/projectid/1000"))
View(res.sample)
res.extract <- as.data.frame(fromJSON("http://localhost:5000/sampleid/30160"))
View(res.extract)
"""
@app.route('/projectid/<int:projectid>', methods=['GET'])
def get_sample(projectid):
    res = wsdl_sample(projectid)

    if len(res) == 0:
        return jsonify({'error': 'no resources found.'})
        # abort(404)

    return jsonify({'sample': res})


"""
generic query interface

example:

R> 
"""
@app.route('/q', methods=['GET', 'POST'])
def q():
    try:
        content = json.loads(request.data)
    except:
        return jsonify({'error': 'could not get POST content.'})

    
    bf = bfabric.Bfabric(login = content['login'], password = content['webservicepassword']) 

    for i in content.keys():
        print "{}\t{}".format(i, content[i])

    res = bf.read_object(endpoint=content['endpoint'][0], obj=content['query'])

    try:
        return jsonify({'res': res})
    except:
        return jsonify({'status': 'failed'})

@app.route('/query', methods=['GET', 'POST'])
def query():
    try:
        content = json.loads(request.data)
    except:
        return jsonify({'error': 'could not get POST content.', 'appid': appid})

    print "PASSWORD CLEARTEXT", content['webservicepassword']
    
    bf = bfabric.Bfabric(login=content['login'], 
      password=content['webservicepassword'], 
      webbase='http://fgcz-bfabric.uzh.ch/bfabric')

    for i in content.keys():
      print "{}\t{}".format(i, content[i])

    if 'projectid' in content:
      workunits = bf.read_object(endpoint='workunit', 
        obj={'applicationid': content['applicationid'],
          'projectid': content['projectid']})
      #workunits = bf.read_object(endpoint='workunit', obj={'applicationid': 205, 'projectid': 1352})
      print workunits
      return jsonify({'workunits': map(lambda x: x._id, workunits)})
    #elif 'query' in content and "{}".format(content['query']) is 'project':
    else:
      user = bf.read_object(endpoint='user', obj={'login': content['login']})[0]
      projects = map(lambda x: x._id, user.project)
      return jsonify({'projects': projects})

    return jsonify({'error': 'could not process query'})
  
@app.route('/addworkunit', methods=['GET', 'POST'])
def add_workunit():
    appid = request.args.get('appid', None)
    pid = request.args.get('pid', None)
    rname = request.args.get('rname', None)

    try:
        content = json.loads(request.data)
        # print content
    except:
        return jsonify({'error': 'could not get POST content.', 'appid': appid})

    resource_base64 = content['base64']
    #base64.b64encode(content)
    print resource_base64 

    #wu = bfapp.save_object(endpoint='workunit', obj={'name': rname, 'applicationid': appid, 'projectid': pid})[0]
    #res = bfapp.save_object('resource', {'base64': resource_base64,
    #                                     'name': 'rname',
    #                                     'workunitid':wu._id})

    #res = bfapp.save_object('workunit', {'id': wu._id, 'status': 'available'})

    return jsonify({'rv': 'ok'})

if __name__ == '__main__':
    #wsdl_user(1000)
    app.run(debug=True, host="0.0.0.0")

