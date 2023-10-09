package bruco;                                                                   
                                                                                 
public class lc {                                                                
    public static int lengthOfLongestSubstring(String s) {                                   
        int res = 0;                                                                         
        String r = "";                                                                       
        String tmp = "";                                                                     
        for(int j = 0; j < s.length(); j++){                                                 
            for(int i = j; i < s.length(); i++){                                             
                if(! tmp.contains(String.valueOf(s.charAt(i)))){                             
                    tmp = tmp + s.charAt(i);                                                 
                    System.out.println(tmp);                                                                        
                    if(tmp.length() > res){    
                        res = tmp.length();                   
                        r = tmp;                                                             
                        //System.out.println(r);                                               
                    }                                                                        
                }else{                                                                       
                    tmp = "";                                                                
                }                                                                            
            }                                            
            tmp = "";                                                                               
        }                                                                                    
        System.out.println(r);                                                               
        return res;                                                                          
    }                                                                           
                                                                     
    public static void main(String[] args){                          
        String f = "jbpnbwwd";                           
        System.out.println(lengthOfLongestSubstring(f));                                                           
                                                                     
    }                                                                
}                                                                    
                                                                     