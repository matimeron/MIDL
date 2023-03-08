Pro tgo

	on_error, 1

;	ene = [4., 5, 6, 7, 8, 9, 10, 11, 12, 3.15, 10./3, 20./3, 25./3, 40./7, $
;			50./7, 50./9,  60./7, 60./11, 70./9, 70./11, 70./13]

	ene = [8.2, 8.4, 8.6, 8.8, 9.2, 9.4, 9.6, 9.8]

	ene = ene(sort(ene))
	for i = 0l, n_elements(ene)-1 do $
	un_make, 3.15, 0.033, !blpar.rgam, 72, 'ua_sn*', !blpar.rsig, !blpar.asig, $
	efir= ene[i], ang= [0.060,0.040], np= [60,40], /auto, /sho
;	un_make, 3.15, 0.033, !blpar.rgam, 72, 'ua_mn*', !blpar.rsig, !blpar.asig, $
;	efir= ene[i], ang= [0.100,0.100], np= [50,50], /auto, /sho

return
end