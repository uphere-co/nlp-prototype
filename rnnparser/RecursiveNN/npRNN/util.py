# -*- coding: utf-8 -*-
import numpy as np

def get_short_uuid(lenth=8):
    import uuid
    return str(uuid.uuid4())[:8]

def write_to_disk(name, arr):
    import h5py
    h5f = h5py.File('/data/groups/uphere/log/rnnparser.h5', 'a')
    h5f.create_dataset(name, data=arr)
    h5f.close()
