import requests
import urllib.request
import time
from bs4 import BeautifulSoup
import lxml.html as lh
import pandas as pd

#To - Do Create a dictionary of the class list and lopp through each one here
#Iowa Standings
#Class A, Class 1A (in the script), Class 2A, Class 3A, Class 4A, Class 8 Man


#First get the list of Conference urls
#headers = {'class-a' : 'class-1a' : 'class-2a' : 'class-3a' : 'class-4a' : 'class-8-man'}

#url = "https://bgp.he.net/report/world"
#html = requests.get(url, headers=headers)

#soup = BeautifulSoup(html.text, 'html.parser')

#table = soup.find('table', {'id':'table_countries'})
#rows = table.find_all('tr')

#country_urls = []

# Go through each row and grab the link. If there's no link, continue to next row
#for row in rows:
#    try:
#        link = row.select('a')[0]['href']
#        country_urls.append(link)
#    except:
#        continue


# Now iterate through that list
#for link in country_urls:

#    url = "https://bgp.he.net" + link
#    html = requests.get(url, headers=headers)
url='https://www.maxpreps.com/division/ia/a1RODbXafUWOvVRSbs79lg/football-fall-20/standings-class-1a.htm'

#Nebraska Standings
#Class A, Class B, Class C1, Class C2, Class D1, Class D2, Class D6
#url = 'https://www.maxpreps.com/division/ne/J_gHA_z1WUOWy7tK2UvZwQ/football-fall-20/standings-class-a.htm'

#Create a handle, page, to handle the contents of the website
page = requests.get(url)

#Store the contents of the website under doc
doc = lh.fromstring(page.content)

#Parse data that are stored between <tr>..</tr> of HTML
tr_elements = doc.xpath('//tr')

#Create empty list
col=[]
i=0
#For each row, store each first element (header) and an empty list
for t in tr_elements[0]:
    i+=1
    name=t.text_content()
    print('%d:"%s"'%(i,name))
    col.append((name,[]))

#Since out first row is the header, data is stored on the second row onwards
for j in range(1,len(tr_elements)):
    #T is our j'th row
    T=tr_elements[j]
    
    #If row is not of size 10, the //tr data is not from our table 
    if len(T)!=6:
        break
    
    #i is the index of our column
    i=0
    
    #Iterate through each element of the row
    for t in T.iterchildren():
        data=t.text_content() 
        #Check if row is empty
        if i>0:
        #Convert any numerical value to integers
            try:
                data=int(data)
            except:
                pass
        #Append the data to the empty list of the i'th column
        col[i][1].append(data)
        #Increment i for the next column
        i+=1

[len(C) for (title,C) in col]

Dict={title:column for (title,column) in col}
df=pd.DataFrame(Dict)

df.head()

