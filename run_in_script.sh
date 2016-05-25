#!/bin/bash
ipython nbconvert --to python parse_arxiv_meta.ipynb
ipython run_with_cellmagic.ipy
