Pro len4, border = bor

	on_error, 1

	Box, [-1,1], [-1,1], truasp = 'ycen', xmar=[4,4],ymar=[2,2], border = bor

	Arc_mm, [0.3,-0.3],[0.3,0.3], rad=0.9, shape=sh1, /no
	Arc_mm, [0.3,0.3],[0.3,-0.3], rad=0.9, shape=sh2, /no
	shap = transpose([transpose(sh1),transpose(sh2)])
;	shap = Shape_trans(shap,15,1,/deg,cen=[0.3,0])
	polyfill, shap, col = Grey(0.8)
	plots, shap, thi=2, col=Grey(0.5)
	Arro, from=[-0.9,-0.1],to=[-0.9,0.1],/two, thi=3, col=!pcol.dred,siz=0
	Arro, from=[-0.9,0], to= [-0.2,0], siz=1.5, thi=2
	Arro, from=[-0.2,0], to=[0.3,0], siz=1.5, thi=2, line=2
	Arro, from=[-0.2,0], to= [0.3,0.15], siz=1.5, thi=2
	Arro, from=[-0.9,-0.15*7./5], to=[-0.2,0], siz=0,thi=2, line=2

	Arc_mm, [0.22,0.], cen=[-0.2,0],ang=18,/deg, line=1, thi=2
	Labels, [0.15],[0.025],['!7Dh!x'],$
		align=0.5, charsize=1.8,charthi=1.5	
	psyms
	
	return
end