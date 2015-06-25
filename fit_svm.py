import numpy as np
from sklearn import svm, grid_search

def fit_ipw_svm(x, y, ts, weights, C):
	#convert to numpy arrays
	nX = np.array(x,ndmin=2)
	ntsX = np.array(ts,ndmin=2)
	nY = np.array(y,ndmin=1)/1.0
	#for tuning
	paramGrid = [
	{'C': C, 'kernel': ['linear']}
	]
	#train
	trnMdl = svm.SVC()
	trnMdl_grid = grid_search.GridSearchCV(trnMdl, paramGrid, fit_params={'sample_weight': weights})
	trnMdl_grid.fit(X=nX, y=nY)
	trnMdl_best = trnMdl_grid.best_estimator_
	#return SVM weights and predictions
	trnMdl_rho = trnMdl_best.intercept_[0:1]
	trnMdl_rho = list(trnMdl_rho)
	trnMdl_w = trnMdl_best.coef_[0,:]
	trnMdl_w = list(trnMdl_w)
	yhats = trnMdl_best.predict(ntsX)
	yhats = list(yhats)
	return trnMdl_w, trnMdl_rho, yhats

