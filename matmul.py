import numpy as np  
import time   
N = M = K = 8192  
  
def get_gflops(M, N, K):  
    return M*N*(2.0*K-1.0) / 1000**3  
  
A = np.matrix(np.random.random((M, N)), copy=False)  
B = np.matrix(np.random.random((N, K)), copy=False)  
  
C = A*B  
  
start = time.time()  
for i in range(5):  
    C = A*B
end = time.time()  
tm = (end-start) / 5.0  
print ('{0:4}, {1:9.7}, {2:9.7}'.format(K, tm, get_gflops(M, N, K) / tm)) 

