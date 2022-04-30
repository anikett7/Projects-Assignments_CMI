#Spazer tool for processing web pages
#

from bs4 import BeautifulSoup
import pathlib
import re
address=re.compile(r"address|Address|ADDRESS|contact|CONTACT|Contact(s)")
pincode=re.compile(r"\b\d\d\d(\s)*\d\d\d\b")

#Variables to track the input, output and gained space
space_gained = 0
space_input = 0
space_output = 0

print("Welcome to Spazer\n")

for x in range(10):
    filename = str(x) + ".html"
    file = pathlib.Path('input/' + filename)
    if (file.exists()):

        #Read each file
        print("Reading " + filename)
        f = open('input/' + filename, 'r', errors="ignore")
        contents = f.read()   
        
        #Remove html tags
        soup=BeautifulSoup(contents,'html.parser')
        addtags=soup.find_all("address")
        output1=""
        for addtag in addtags:
            output1=output1+addtag.get_text()
        addclasses=soup.find_all(class_=re.compile(r".*(address|Address|ADDRESS).*"))
        for addclass in addclasses:
            output1=output1+addclass.get_text()
        addids=soup.find_all(id=re.compile(r".*(address|Address|ADDRESS).*"))
        for addid in addids:
            output1=output1+addid.get_text()
        outputs3 = soup.find_all(re.compile(r".*"), string=re.compile(r".*(address|Address|ADDRESS).*"))
        temp=""
        for output3 in outputs3:
            temp=temp+output3.parent.get_text()
            
        for i in re.finditer(address,temp):
            output1=output1+(temp[(i.start()):(i.start()+300)]).strip()
        for i in re.finditer(pincode,temp):
            output1=output1+(temp[(i.start()-200):(i.start()+20)]).strip()
            
        if len(output1)<50:
            soup = BeautifulSoup(contents, 'html.parser')        
            output2 = soup.get_text() 
            temp=""
            for i in re.finditer(address,output2):
                temp=temp+(output2[(i.start()):(i.start()+300)]).strip()
            for i in re.finditer(pincode,output2):
                temp=temp+(output2[(i.start()-200):(i.start()+20)]).strip()
            output1 = temp
               
         
       
        #Your code begins  ###############################
        
        
        #Your code ends  #################################              
        
        #Write the output variable contents to output/ folder.
        print ("Writing reduced " + filename)
        fw = open('output/' + filename, "w")
        fw.write(output1)
        fw.close()
        f.close()
        
        #Calculate space savings
        space_input = space_input + len(contents)
        space_output = space_output + len(output1)
 
        
space_gained = round((space_input - space_output) * 100 / space_input, 2)

print("\nTotal Space used by input files = " + str(space_input) + " characters.") 
print("Total Space used by output files = " + str(space_output) + " characters.")
print("Total Space Gained = " + str(space_gained) + "%") 
       
    




