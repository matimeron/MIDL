Pro len2, border = bor

	on_error, 1

	Box, [-1,1], [-1,1], truasp = 'ycen', xmar=[4,4],ymar=[2,2], border = bor

	Arc_mm, [0.3,-0.3],[0.3,0.3], rad=0.9, shape=sh1, /no
	Arc_mm, [0.3,0.3],[0.3,-0.3], rad=0.9, shape=sh2, /no
	shap = transpose([transpose(sh1),transpose(sh2)])
	shap = Shape_trans(shap,0,1,[0,.1])
	polyfill, shap, col = Grey(0.8)
	plots, shap, thi=2, col=Grey(0.5)
	Arro, from=[-0.9,-0.1],to=[-0.9,0.1],/two, thi=3, col=!pcol.dred,siz=0
	psyms
	Arro, from=[-0.9,0], to= [0.3,0], siz=1.5, thi=2

	plots, [0,0.3], [0.1,0.1], line=1, thi=2
	Arro, from = [0,0.1], to=[0,0], /two
	Labels, [-0.06],[0.04],['!7D!xx'],$
		align=0.5, charsize=1.8,charthi=1.5	
	psyms
	
	return
end