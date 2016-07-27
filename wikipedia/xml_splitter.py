import json
import sys
import logging
from xml.etree.cElementTree import iterparse, tostring

def set_logger(logger):
    log_path = "/data/groups/uphere/log/script.log"
    logger.setLevel(logging.DEBUG)
    fh = logging.FileHandler(log_path)
    formatter = logging.Formatter('%(asctime)s\t%(name)s\t%(levelname)s\t%(message)s')
    fh.setFormatter(formatter)
    logger.addHandler(fh)

def get_short_uuid(lenth=8):
    import uuid
    return str(uuid.uuid4())[:8]

if __name__ == "__main__":
    config_path=sys.argv[1]
    with open(config_path, 'r') as f:
        config=json.load(f)

    logger=logging.getLogger("XMLsplitter#%s"%get_short_uuid())
    set_logger(logger)

    input_file = config['input']
    tag_name   = config['tag']
    n_item     = config['item_per_file']

    logger.info("Task launched.")
    context = iter(iterparse(input_file, events=("start", "end")))
    event, root = next(context)
    i=0
    content=''
    for event, elem in context:
       try:
           if event=="end" and elem.tag[-len(tag_name):] == tag_name:
               #content += tostring(elem)
               content += elem.text.encode('utf-8')+'\n'
               if i>0 and i%n_item==0:
                   with open(input_file+'.%s.%d'%(tag_name, i/n_item),'w') as f:
                       f.write(content)
                       content=''
                       logger.info("%d-th element is processed."%i)
               root.clear()
               i+=1
       except:
            logger.debug("Exception raised. ")
    logger.info("Task finished.")
