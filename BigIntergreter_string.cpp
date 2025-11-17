// BigIntergreter On C++
//multyply : Divide and Conquer (FFT : later.)
// made by 0B42(ZeroBase42)
// string ver.
#include<iostream>
#include<string>
#include<algorithm>
#include<vector>
#include<string.h>
using namespace std;

void trimLeadingZeros(string& s){
    int p=s.find_first_not_of('0');
    if(p==string::npos)s="0";
    else s.erase(0,p);
}
void erase0(string& a, string& b){
    trimLeadingZeros(a);trimLeadingZeros(b);
}
bool neg(string s){ // negative : 1, not : 0
    return !s.empty()&&s[0]=='-';
}

string maxS(string a,string b){ // return Big number
	bool na=neg(a),nb=neg(b);
    if(na&&!nb)return b;
    if(!na&&nb)return a;
	string A=na?a.substr(1):a,B=nb?b.substr(1):b;
	erase0(A,B);
    if(A.empty())A="0";if(B.empty())B="0";
    if(A.size()!=B.size())return(A.size()>B.size())^(na)?a:b;
    if(A!=B)return(A>B)^na?a:b;
    return a;
}
int absS(string a,string b){
	erase0(a,b);
    // A>B :1, A==B:0,A<B: -1
	if (a.size()>b.size())return 1;
    if (a.size()<b.size())return -1;
    if (a==b)return 0;
    return (a>b?1:-1);
}
string addS(string a,string b){ // add A,B (0<=A,B)(not negative)
    string r;int i=(int)a.size()-1,j=(int)b.size()-1,c=0;
    while(i>=0||j>=0||c){
        c+=(i>=0?a[i--]-'0':0)+(j>=0?b[j--]-'0':0);
        r+=c%10+'0';c/=10;
    }
    reverse(r.begin(),r.end());
    return r;
}

// ^^ preprocessing code ^^
string subS(string a,string b){
    bool na=neg(a),nb=neg(b);
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
	bool na=neg(a),nb=neg(b);
	string A=na?a.substr(1):a,B=nb?b.substr(1):b;
	if(!na&&!nb)return addS(A,B);
	if(na&&nb)return"-"+addS(A,B);
	int cmp=absS(A,B);if(!cmp)return"0";
	return(cmp>0?(na?"-":"")+subS(A,B):(nb?"-":"")+subS(B,A));
}
string mulS(string a,string b){
    bool k=0;
	if(a[0]=='-')k^=1,a=a.substr(1);
	if(b[0]=='-')k^=1,b=b.substr(1);
	if(a=="0"||b=="0")return"0";
	int n=a.size(),m=b.size();
    int r[n+m];
    memset(r,0,sizeof(int)*(n+m));
	for(int i=0;i<m;i++)
		for(int j=0;j<n;j++){
			int t=(a[n-1-j]-'0')*(b[m-1-i]-'0');
			r[n+m-1-i-j]+=t;
		}
	for(int i=n+m-1;i>0;i--)r[i-1]+=r[i]/10,r[i]%=10;
	bool f=0;
	string s="";
	for(int i=0;i<n+m;i++){
		if(!f){
			if(r[i]!=0)f=1;
			else if(i==n+m-1)s+="0";
		}
		if(f)s+=to_string(r[i]);
	}
  return k?"-"+s:s;
}
// need to re-pare
string divS(string a,string b){
	bool n=0;
	if(a[0]=='-')n^=1,a=a.substr(1);
	if(b[0]=='-')n^=1,b=b.substr(1);
	if(b=="0")throw out_of_range("DivideByZeroError");
	if(absS(a,b)<0)return "0";
	string r="",t="0";
	for(char c:a){
		t+=c;t.erase(0,t.find_first_not_of('0'));
		if(t=="")t="0";
		int x=0;
		while(absS(t,b)>=0){t=subS(t,b);x++;}r+=x+'0';
	}
    r.erase(0,r.find_first_not_of('0'));
    if(r=="")r="0";
    return n?"-"+r:r;
}
string modS(string a,string b){
    bool n=a[0]=='-';
	if(a[0]=='-')a=a.substr(1);
	if(b[0]=='-')b=b.substr(1);
    string t="0";
    for(char c:a){
		t+=c;t.erase(0,t.find_first_not_of('0'));
		if(t=="")t="0";
		while(absS(t,b)>=0)t=subS(t,b);
	}
    if(t=="")t="0";
	return n && t!="0"?"-"+t:t;
}