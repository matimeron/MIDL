Pro DCD_pic_n3, bor=bor

	on_error, 1

	window, 0, xsize=512, ysize = 512

	Plvar_keep, act='sav'
	!x.margin = [4,4]
	Box, [-0.6,0.6], [-0.6,0.6], border=bor
	pcen = [0,0]
	cen = [0.1,0.05]
	Circle_mm, cen= cen, rad=0.5, thi=3
	plots, pcen, psym=8, thi=2, symsize=1.5
	plots, cen, psym=8, thi=2, symsize=1.5

	s = -[0.4,0.5]
	Arro, from=s, to=s+[0.15,0]
	Arro, from=s, to=s+[0,0.15]
	psyms
	
	Labels, [pcen[0]-0.07,cen[0]+0.07],[pcen[1]-0.04,cen[1]+0.02], $
		align=0.5,charsize=2.5,charthi=1.5, $
		['C!d!7u!x!n','C!d!7v!x!n']
	Labels, [s[0]+0.1,s[0]-0.03],[s[1]-0.04,s[1]+0.09],align=0.5,['x','z'],$
		charsize=1.5,charthi=1.5

	Plvar_keep, act='res'

	return
end