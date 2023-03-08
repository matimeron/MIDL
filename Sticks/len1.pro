Pro len1, border = bor

	on_error, 1

;	Box, [-1,1], [-1,1], truasp = 'ycen', xmar=[4,4],ymar=[2,2], border = bor
	Box, [-1,1], [-1,1], truasp = 'ycen', border = bor

	Arc_mm, [0.3,-0.3],[0.3,0.3], rad=0.9, shape=sh1, /no
	Arc_mm, [0.3,0.3],[0.3,-0.3], rad=0.9, shape=sh2, /no
	shap = transpose([transpose(sh1),transpose(sh2)])
	polyfill, shap, col = Grey(0.8)
	plots, shap, thi=2, col=Grey(0.5)
	Arro, from=[-0.9,-0.1],to=[-0.9,0.1],/two, thi=3, col=!pcol.dred,siz=0
	Arro, from=[0.9,-0.05],to=[0.9,0.05],/two, thi=3, col=!pcol.dred,siz=0
	psyms
	Arro, from=[-0.9,0], to= [0.3,0], siz=1.5, thi=2
	Arro, from=[0.3,0], to= [0.9,0], siz=1.5, thi=2
	Arro, from=[-0.9,0], to= [0.3,0.2], siz=1.5, thi=2
	Arro, from=[0.3,0.2], to= [0.9,0],siz=1.5, thi=2
	plots, [-0.9,-0.9],[-0.1,-0.5], line=1,thi=2
	plots, [0.3,0.3],[-0.3,-0.5], line=1,thi=2
	plots, [0.9,0.9],[-0.05,-0.5], line=1,thi=2
	Arro, from=[-0.9,-0.5], to=[0.3,-0.5], /two
	Arro, from=[0.3,-0.5], to=[0.9,-0.5], /two
	Labels, [-0.3, 0.3,0.6],[-0.58,0.33,-0.58],['L!d1!n','F','L!d2!n'],$
		align=0.5, charsize=1.8,charthi=1.5	
	psyms
	
	return
end