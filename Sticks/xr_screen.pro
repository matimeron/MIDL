Pro XR_Screen, ang, border = bor

	on_error, 1

	Box, [-1,1], [-1,1], tru='ycen', xmar=[3,3], ymar=[2,2], border=bor

	Plvar_keep, act='sav'

	xli = [-0.8,0.8]
	yli=[-0.6,0.6]	
	Rectan, xli = xli, yli=yli, thi = 4
	Circle_mm, cen = [-0.25,0.07], rad = 0.1, /no_show, shape=shap
	polyfill, shap, col=!pcol.cyan
	
	for i = -2, 2 do plots, xli, i*[0.2,0.2], thi=2*(1+(i eq 0)), line=1
	for j = -3, 3 do plots, j*[0.2,0.2], yli, thi=2*(1 + (j eq 0)), line=1

	Plvar_keep, act='res'

	return
end