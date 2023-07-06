## Bitcoin Network

## Table of Contents

 - [Bitcoin Network](#bitcoin-network)
- [1. Problem Statement](#1-problem-statement)
- [2. Data Description](#2-data-description)
  * [Attribute Information](#attribute-information)
- [3. EDA](#3-eda)
- [4. Modelling Evaluation](#4-modelling-evaluation)
- [5. Results](#5-results)

### Bitcoin Network

This is who-trusts-whom network of people who trade using Bitcoin on a platform called Bitcoin OTC.
Since Bitcoin users are anonymous, there is a need to maintain a record of users’ reputation to prevent transactions with fraudulent and risky users.
Members of Bitcoin Alpha rate other members in a scale of -10 (total distrust) to +10 (total trust) in steps of 1.
This is the **weighted signed directed network**.

### 1. Problem Statement
Indentify the communities in the Bitcoin transaction

### 2. Data Description
Data is obtained from  [here](https://snap.stanford.edu/data/soc-sign-bitcoin-otc.html).

- Number of instances - 35,592
- Number of features - 4
  #### Attribute Information
  - **SOURCE**:node id of source (i.e rater)
  - **TARGET**:node id of target (i.e ratee)
  - **RATING**:the source’s rating for the target, ranging from -10 to +10 in steps of 1
  - **TIME**:the time of the rating, measured as seconds since Epoch.
  

   
  
 ### 3. EDA
 <p float="left">
  <img src="https://github.com/gabrielecola/Bitcoin_Network/assets/103529789/3ce978e0-3e5d-4b91-9975-2c7c0fa76808" width="380"/>
  <img src="https://github.com/gabrielecola/Bitcoin_Network/assets/103529789/95dd9305-e555-436a-98f3-4fa03e1e8bf0" width="350"/>
  </p>

  


  
  
 ### 4. Modelling Evaluation
 - Algorithms used
    - Walk-Trap
 - Metrics used: Modularity
 
  ### 5. Results
  
   <p float="left">
  <img src="https://github.com/gabrielecola/Bitcoin_Network/assets/103529789/312fa8d0-6b5e-4918-9cd0-194e5ce83382" width="450"/>
  </p>

