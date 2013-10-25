import math,itertools,csv

#var = int(raw_input())

def isprime(y):
	if y==1:
		return False
	else:
		def check1(y,x):
			return not (y%x==0)
		allTrue=all(check1(y,x) for x in range(2,int(math.floor(math.sqrt(y)))+1))
		return allTrue

#isprime(var)

def create(x,s):
	y=x[-1]
	y1=y+2*s
	y2=y1+2*s
	y3=y2+2*s
	y4=y3+2*s
	x.append(y1)
	x.append(y2)
	x.append(y3)
	x.append(y4)
	return x
sum1=1
m=1
mylist=[1]
while mylist[-1]<(1001*1001):
	mylist=create(mylist,m)
	m=m+1
	sum1=sum(mylist)

#print sum1	
####################################################################
#Euler 29
list1=[a**b for a in range(2,101) for b in range(2,101)]
list1=list(set(list1))
#print len(list1)
#####################################################################
#euler30
def tolist(x):
	list123=[]
	while x>0:
		x1=x%10
                list123.append(x1)
                x=int(x/10)
	list123.reverse()
	return list123

def satisfy(x):
	list123=[]
	temp=x
	list123=tolist(x)
	list123=map(lambda x:x**5,list123)
	y= sum(list123)
	return (y==temp)
	

#print satisfy(1634)
def euler29():
	list1=[x for x in range(2,360000) if satisfy(x)]
	print sum(list1)

############################################################
def total(a,b,c,d,e,f,g):
	h=200-(a*200+b*100+c*50+d*20+e*10+f*5+g*2)
	return h
def euler30():
	
	list1=[[a,b,c,d,e,f,g] for a in range(0,2) for b in range(0,3) for c in range(0,5) for d in range(0,11) for e in range(0,21) for f in range(0,41) for g in range(0,101) if total(a,b,c,d,e,f,g)>=0 ]
	print len(list1)
###########################################################################
def checkfrac(x,y):
	if x<y:
		x1=int(x/10)
		y1=int(y/10)
		x2=x-x1*10
		y2=y-y1*10
		if y1==x2 and y1!=0 and y2!=0:
			return ((x/float(y))==(x1/float(y2)))
		else:
			return False
	else:
		return False
def euler33():
	list1=[(x,y) for x in range(10,100) for y in range(10,100) if (checkfrac(x,y)) ]
	print list1

###############################################################################
def fact(n):
	if n==0:
		return 1
	else:
		return n*fact(n-1)
def facttest(x):
	list23=tolist(x)
	list23=map(fact,list23)
	return (sum(list23)==x)	

def euler34():
	list1=[x for x in range(2,255000) if facttest(x)]
	print list1
###############################################################################

#list1=[x for x in range(2,1000000) if isprime(x)]
list2=range(2,1000000)
upper_limit=math.floor(math.sqrt(1000000))
def prime(x,ind):
	while x[ind]<upper_limit:
		var=x[ind]
		x=filter(lambda y:(y%var!=0 or y==var),x)	
		ind=ind+1
	return x
def strtonum(listno):
	no=0
	for i in range(0,len(listno)):
		no=10*no+int(listno[i])
	return no		
def euler35():
		def rotate(listprime):
			ind=0
			ans=[]
			while ind<len(listprime):
				oldno=listprime[ind]
				str1=tolist(oldno)
				newstr1=[]
				count=0
				ind1=0
				while ind1<(len(str1)+1):	
					newstr1=str1[1:]
					newstr1.append(str1[0])
					no=strtonum(newstr1)
					if isprime(no):
						str1=newstr1
						newstr1=[]				
					else:
						count=1
						break
					ind1=ind1+1
				if count==0:
					ans.append(oldno)
				ind=ind+1
			return ans
		ans1=rotate(prime(list2,0))
		print ans1,len(ans1)

################################################################################################################

def checkinit(x):
	var=10
	while x%var!=x:
		temp=x%var
		temp2=x/var
		if isprime(temp) and isprime(temp2):
			pass
		else:
			return False
		var=var*10
	return True

def euler38():
	print sum(filter(checkinit,prime(list2,0))[4:])


##########################################################################
def euler40():
	str1="0"
	i=1
	j=1
	sum1=1
	while len(str1)<1000001:
		str1=str1+str(i)	
		i=i+1	
	while j<=1000000:
		sum1=sum1*int(str1[j])
		j=j*10
	print sum1
#euler40()

###########################################################################
def euler41():
	i=0
	str1='7654321'
	list1=map(list , list(itertools.permutations(str1, len(str1))))
	while i<(len(list1)):
		no=strtonum(list1[i])		
		if isprime(no):
			return no
		else:
			i=i+1	
	return 0

#print euler41()

#############################################################################
def toint(x):
	return sum(map(lambda x: ord(x)-ord('A')+1,x))
def euler42():
	spam=csv.reader(open("words.csv",'rb'),delimiter=',')
	for row in spam:
		list1=row
	list1=map(toint,list1)
	list2=[x*(x+1)/2 for x in range(1,21)]	
	list1=filter(lambda x: x in list2,list1)
	print len(list1)
