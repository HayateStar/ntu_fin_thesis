# ntu_fin_thesis


### 9/5 Update 

* 成分股與財報公司數目比較 : 比較去年底財報產業別公司數目與目前各產業成分股公司數目，藉此看有沒有產業下市或停止交易等。
* 成分股變化公司 : 指雖有在年底財報上市公司中出現，但目前並不存在於產業別指數之成分股公司。
* 整理 : 整理出上述csv的 R Code
* Note : 目前先想確認作法是否正確，再看看要不要個別找出下市或未列入成分股成因。





### 10/11 Update : EM & AQ 計算

* 使用資料庫 : TEJ IFRS以合併為主財務(累計)-一般產業(IV)
* 資料期間 : 2008-2018 (IFRS採用後)

* 計算指標：
  * EM : Jones Model, Modified Jones Model, Performance Matching Model
  * AQ : Persistence, Differential persistence, Standard Deviation of Accruals, Standard Deviation of Accruals Residuals (DD Measure)

* 計算檔案：EM & AQ計算.R

* 計算結果：(以2016年上市半導體業為例)
  
  *  x軸 : DD Measure , y軸 : Performance Matching
![image](https://github.com/HayateStar/ntu_fin_thesis/blob/master/graph/1011_test_%E5%8D%8A%E5%B0%8E%E9%AB%94_1.png)

  *  x軸 : DD Measure , y軸 : Modified Jones Model
![image](https://github.com/HayateStar/ntu_fin_thesis/blob/master/graph/1011_test_%E5%8D%8A%E5%B0%8E%E9%AB%94_2.png)

  *  x軸 : Differential Persistence, y軸 : Performance Matching
 ![image](https://github.com/HayateStar/ntu_fin_thesis/blob/master/graph/PM%20vs%20DP.png)

  *  x軸 : Differential Persistence , y軸 : Modified Jones Model
![image](https://github.com/HayateStar/ntu_fin_thesis/blob/master/graph/2016%E5%8D%8A%E5%B0%8E%E9%AB%94%20(DP%20vs%20MJM).png)
