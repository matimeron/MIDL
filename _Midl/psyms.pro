Pro Psyms, num, size = siz, color = col, ps = ps

	on_error, 1

	siz = Default(siz,1)

	phi1 = 2*!pi*findgen(8)/8
	phim = 2*!pi*findgen(12)/12
	phi2 = 2*!pi*findgen(16)/16
	sh1 = Shape_close(transpose([[cos(phi1)],[sin(phi1)]]))
	shm = Shape_close(transpose([[cos(phim)],[sin(phim)]]))
	sh2 = Shape_close(transpose([[cos(phi2)],[sin(phi2)]]))
	case Default(num,0,/dtyp) of
		0	:	sym = [[sh2],[0.66*sh2],[0.33*sh1]]
		1	:	begin
					sh1m = sh1
					for i = 1,7,2 do sh1m[*,i] = 0.33*sh1m[*,i]
					sym = [[sh1],[0.66*sh1m]]
				end
		2	:	begin
					sh2m = sh2
					for i = 2,14,4 do sh2m[*,i] = 0.25*sh2m[*,i]
					sym = [[sh2],[0.66*sh2m]]
				end
		3	:	begin
					phi = 2*!pi*shift(findgen(16),-1)/16
					sh = Shape_close(transpose([[cos(phi)],[sin(phi)]]))
					sh[*,1] = [1.2,1.2]
					sh[*,5] = [-1.2,1.2]
					bas = sh[*,8]
					dif = sh[*,9] - sh[*,8]
					tem = [[bas + 0.1*dif],[bas + 0.2*dif],[bas + 0.3*dif]]
					lm =[[-1.5,-0.4],[tem[*,0]],[-1.45,-0.5],[tem[*,1]],$
						[-1.4,-0.6],[tem[*,2]]]
					bas = sh[*,14]
					dif = sh[*,13] - sh[*,14]
					tem = [[bas + 0.1*dif],[bas + 0.2*dif],[bas + 0.3*dif]]
					rm =[[1.5,-0.4],[tem[*,0]],[1.45,-0.5],[tem[*,1]],$
						[1.4,-0.6],[tem[*,2]]]
					sh =[[sh[*,0:8]],[lm],[sh[*,9:14]],[rm],[sh[*,15:16]]]
					fc =[[0.6,0.2],[0.45,0.2],[0.3,0.15],[0.2,0.0],[-0.2,0.0],$
					  [-0.3,0.15],[-0.45,0.2],[-0.6,0.2],[sh[*,6]],$
					  [-0.6,0.2],[-0.45,0.0],[-0.3,-0.05],[-0.2,0.0],[0.2,0.0],$
					  [0.3,-0.05],[0.45,0.0],[0.6,0.2],[sh[*,0]]]
					sym = [[sh],[fc]]
				end
		4	:	sym = [[sh2],[0.5*sh2]]
		5	:	sym = sh2
		6	:	sym = [[sh2],[0.9*shm],[0.8*sh2]]
		else:	message, "There ain't no such symbol!"
	endcase

	fil = 1 - keyword_set(ps)
	if n_elements(col) eq 3 then usersym,siz*sym,fill=fil,col=Trucol(col,/max) $
	else usersym, siz*sym, fill = fil

	return
end