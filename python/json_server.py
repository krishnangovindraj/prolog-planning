# This is just to sanity check requests made from prolog.
# Stolen from : https://gist.github.com/nitaku/10d0662536f37a087e1b, updated to python3

from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import cgi

from pj_task_interface import PJTaskInterface

from pj_protocol import PrologJSONProtocolObject

# https://docs.python.org/3/library/json.html
class PrologJSONProtocolObjectEncoder(json.JSONEncoder):
     def default(self, obj):
         if isinstance(obj, PrologJSONProtocolObject):
             return obj.to_dict() 
         else:# Let the base class default method raise the TypeError
            return json.JSONEncoder.default(self, obj)


class Server(BaseHTTPRequestHandler):
    def _set_headers(self):
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        
    def do_HEAD(self):
        self._set_headers()
        
    # GET sends back a Hello world message
    def do_GET(self):
        # Can you think of any uses for a get endpoint? :|
        self._set_headers()
        self.wfile.write(json.dumps({'hello': 'world', 'received': 'ok'}, cls=PrologJSONProtocolObjectEncoder).encode('utf-8'))
        
    # POST echoes the message adding a JSON field
    def do_POST(self):
        ctype, pdict = cgi.parse_header(self.headers.get('content-type'))
        
        # refuse to receive non-json content
        if ctype != 'application/json':
            self.send_response(400)
            self.end_headers()
            return
            
        # read the message and convert it into a python dictionary
        length = int(self.headers.get('content-length'))
        request_body = json.loads(self.rfile.read(length))
        
        # add a property to the object, just to mess with data
        response_body = PJTaskInterface.process_request(request_body)
        
        # send the message back
        self._set_headers()
        self.wfile.write(json.dumps(response_body, cls=PrologJSONProtocolObjectEncoder).encode('utf-8'))
        
def run(server_class=HTTPServer, handler_class=Server, port=8008):
    server_address = ('', port)
    httpd = server_class(server_address, handler_class)
    
    print('Starting httpd on port %d...'%port)
    httpd.serve_forever()
    
if __name__ == "__main__":
    from sys import argv
    
    if len(argv) == 2:
        run(port=int(argv[1]))
    else:
        run()