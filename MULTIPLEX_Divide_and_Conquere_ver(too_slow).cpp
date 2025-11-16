#include<string>
#include<algorithm>
using namespace std;

string maxS(string a,string b){
	bool na=a[0]=='-',nb=b[0]=='-';
    if(na&&!nb){return b;}
    if(!na&&nb){return a;}
	string A=na?a.substr(1):a,B=nb?b.substr(1):b;
	A.erase(0,A.find_first_not_of('0'));B.erase(0,B.find_first_not_of('0'));
    if(A.empty()){A="0";}
    if(B.empty()){B="0";}
    if(A.size()!=B.size()){return A.size()>B.size()^(na)?a:b;}
    if(A!=B){return A>B^na?a:b;}
    return a;
}
int absS(string a,string b){
	a.erase(0,a.find_first_not_of('0'));b.erase(0,b.find_first_not_of('0'));
	if(a.size()!=b.size()){return a.size()>b.size()?1:-1;}
	return a.compare(b);
}
string addS(string a,string b){
    string r;int i=(int)a.size()-1,j=(int)b.size()-1,c=0;
    while(i>=0||j>=0||c){
        c+=(i>=0?a[i--]-'0':0)+(j>=0?b[j--]-'0':0);
        r+=c%10+'0';c/=10;
    }
    reverse(r.begin(),r.end());
    return r;
}
string subS(string a,string b){
    bool na=a[0]=='-',nb=b[0]=='-';
    if(na&&nb)return subS(b.substr(1),a.substr(1));
    if(na&&!nb)return "-"+addS(a.substr(1),b);
    if(!na&&nb)return addS(a,b.substr(1));
    int c=absS(a,b);
    if(!c)return"0";
	string r;
    if(c>0){
		int i=a.size()-1,j=b.size()-1,cc=0;
		while(i>=0){
			int x=a[i--]-'0'-(j>=0?b[j--]-'0':0)-cc;
			cc=x<0?1:0;if(x<0)x+=10;r+=x+'0';
		}
	}
    else{
		int i=b.size()-1,j=a.size()-1,cc=0;
		while(i>=0){
			int x=b[i--]-'0'-(j>=0?a[j--]-'0':0)-cc;cc=x<0?1:0;
			if(x<0)x+=10;r+=x+'0';
		}
	}
	while(r.size()>1&&r.back()=='0')r.pop_back();
	reverse(r.begin(),r.end());return c>0?r:"-"+r;
}

string sumS(string a,string b){
	bool na=a[0]=='-',nb=b[0]=='-';
	string A=na?a.substr(1):a,B=nb?b.substr(1):b;
	if(!na&&!nb){return addS(A,B);}
	if(na&&nb){return"-"+addS(A,B);}
	int cmp=absS(A,B);if(!cmp){return"0";}
	return(cmp>0?(na?"-":"")+subS(A,B):(nb?"-":"")+subS(B,A));
}

string mulS(string a,string b){
    bool n=0;
	if(a[0]=='-')n^=1,a=a.substr(1);
	if(b[0]=='-')n^=1,b=b.substr(1);
	if(a=="0"||b=="0")return"0";
    string r="0";int m=a.size(),p=b.size();
    for(int i=p-1;i>=0;i--){
        string t="";int c=0;
        for(int j=m-1;j>=0;j--){int x=(b[i]-'0')*(a[j]-'0')+c;c=x/10;t=char(x%10+'0')+t;}
        if(c) t=char(c+'0')+t;
        t+=string(p-1-i,'0');
        r=sumS(r,t);
    }
    return n?"-"+r:r;
}