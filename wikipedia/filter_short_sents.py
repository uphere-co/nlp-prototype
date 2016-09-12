import sys

#infile='/data/groups/uphere/ptb3/wsj.val'
infile=sys.argv[1]
outfile=infile+'.short_sents'
with open(infile, 'r') as f:
    with open(infile+'.tree','r') as f2:
        lines=f.readlines()
        lines2=f2.readlines()
is_short=[len(x.split())<16 for x in lines]
with open(outfile, 'w') as f:
    with open(outfile+'.tree', 'w') as f2:
        for i, (sent, tree) in zip(is_short, zip(lines, lines2)):
            if not i :
                continue
            f.write(sent)
            f2.write(tree)
