import csv

from pattern.web import URL, DOM, plaintext, strip_between
from pattern.web import NODE, TEXT, COMMENT, ELEMENT, DOCUMENT


#For the 2013 datasheet, use this code: 

url = URL('http://www.satp.org/satporgtp/countries/pakistan/database/majorincidents.htm')
dom = DOM(url.download(cached=True))

myarray = []
tab = dom.by_tag('table')
for i in dom.by_tag('td')[11:]:
    g = i.content
    h = plaintext(g)
    myarray.append(h)

def chunks(l, n):
    return [l[i:i+n] for i in range(0, len(l), n)]

yes = chunks(myarray, 5)

output = open("satpincidents2013.csv", "wb")

writer = csv.writer(output)

for i in yes[0:]:
    SN = i[0]
    Date = i[1]
    Incidents = i[2]
    Killed = i[3]
    Injured = i[4]
    writer.writerow([SN, Date, Incidents, Killed, Injured])

output.close()

#For all datasheets before 2013, use the code below


array12 = []
url12 = URL('http://www.satp.org/satporgtp/countries/pakistan/database/majorincidents2012.htm')
dom12 = DOM(url12.download(cached=True))

for i in dom12.by_tag('p')[1:]:
    g = i.content
    h = plaintext(g)
    array12.append(h)

# print array12
new = []
for i in array12:
    new.append(i.split(":"))

print new
