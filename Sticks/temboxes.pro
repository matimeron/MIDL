Pro temboxes, border=bor

	on_error, 1

	fir = !pcol.lblue
	sec = !pcol.blue

	Box, [0,10],[0,8], truasp='xlo', bord = bor
	for i = 0, 2 do begin
		for j = 0, 2 do begin
			Square, base = 0.9, ul_corner=[2+i,7-j], thi = 2, col=fir
		endfor
		Square, base = 0.9, ul_corner=[2+i,2], thi = 2, col = fir
	endfor

	for j = 0, 2 do begin
		Square, base = 0.9, ul_corner=[7,7-j], thi = 2, col=sec
		Arro, from=[5.3,6.55-j], to=[6.7,6.55-j],thi=2,size=1.5,col=!pcol.dblue
	endfor
	Square, base = 0.9, ul_corner = [7,2], thi=2, col = sec
	Arro, from=[5.3,1.45], to=[6.7,1.45],thi=2,size=1.5,col=!pcol.dblue

	for j = 0, 4 do begin
		Circle_mm, cent = [3.45,2 + 2.1/6*(j+1)],rad=0.05,/fill, col = fir
		Circle_mm, cent = [7.45,2 + 2.1/6*(j+1)],rad=0.05,/fill, col = sec		
	endfor

	return
end