# $Id: iron.py,v 1.3 2007/08/20 07:02:16 tolpin Exp $

"""
Converts wafer data to the object simulator format.
"""

import sys

# angles 
ANGLES = ['0', '180']

# field positions for the wafer orientations
FIELDS = {'focus': [0,0],
          'x': [3,2],
          'y': [5,4],
          'px': [9,8],
          'py': [11,10] }
                   


class Wsr2s:
    """Data filter, augments each data line with color and site coordinates.
    """
    def __init__(self):
        self.color = None
        self.sitex = None
        self.sitey = None
        
        
    def header(self):
        """Returns hard coded data header, 
        the additional columns for color and site
        are added at the front.
        """
        return [["sitex", "sitey", "focus", "color", "angle", "x", "y", "px", "py" ],
                ["0", "0", "0", "0", "0", "1", "1", "1", "1"]]


    def data(self,line):
        """Classifies a data line, updates the state
        if necessary and returns the line fields as
        a two-dimensional array.
        None means no data -- skip the line.
        @param line: wsr file line
        """
        fields = line.split()
        try: # or not a data line
            int(fields[0])
            return [[str(self.sitex), str(self.sitey),
                     fields[FIELDS['focus'][ia]],
                     self.color,
                     ANGLES[ia],
                     fields[FIELDS['x'][ia]],
                     fields[FIELDS['y'][ia]],
                     fields[FIELDS['px'][ia]],
                     fields[FIELDS['py'][ia]]]
                     for ia in [0,1]]
        except:
            try: # or no fields
                if fields[0]=='TEST':
                    self.color = fields[1]
                    self.site = 0
                elif fields[0]=='SUMMARY':
                    self.sitex, self.sitey = {
                        '-5,0':(-1,1), '-2,2':(0,1), '0,4':(1,1),
                        '-2,-2':(-1,0), '0,0':(0,0), '2,2':(1,0),
                        '0,-4':(-1,-1), '2,-2':(0,-1), '5,0':(1,-1) }[fields[2]]
            except IndexError:
                pass

import sys

def main():
    """Reads wsr file from stdin and prints
    wafer data suitable for input to the toolkit on stdout
    """
    wsr2s = Wsr2s()
    for hline in wsr2s.header():
        sys.stdout.write(" ".join(hline)+"\n")
    for line in sys.stdin.readlines():
        slines = wsr2s.data(line)
        if slines is not None:
            for sline in slines:
                sys.stdout.write(" ".join(sline)+"\n")

main()
    

            
