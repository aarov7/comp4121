#include<bits/stdc++.h>

using namespace std;

int main(){
    vector<int> arr = {1, 5, 2, 9, 11};
    int newNum;
    cin >> newNum;
    arr.push_back(newNum);
    arr.clear();
    cout << arr.size();
    
    return 0;
}