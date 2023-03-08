Pro wcrysr, border = bor

	on_error, 1
	box, [-1,1], [0,2], truasp='ylo', border = bor, xmar= [3,3], ymar= [2,2]

	for i = 0, 50 do plots, [-1,1], [0.02*i,0.02*i],  col=!pcol.blue, thi=2
	Square, base = 0.1, ll_corner = [-0.05,0.95], /no_show, shap=shap
	n = 12
	h = (2*randomu(s,n) - 1)*0.15
	h = h[sort(h)]
	v = (2*randomu(s,n)-1)*0.05
	for i = 0, n-1 do begin
		tshap = Shape_trans(shap,0,1,[h[i] + (i-n/2)*2./n ,v[i]])
		polyfill, tshap, col = !pcol.white
	endfor
	
	return
end