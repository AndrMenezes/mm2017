proc import datafile = 'C:\Users\User\Dropbox\4° Série\Modelos Mistos\Trabalho\laranja.txt'
			dbms = tab out = dados replace;
			getnames = yes;
run;

ods html; 
proc mixed data = dados;
	class trat bloco planta;
	model diametro = trat avaliacao trat * avaliacao;
	repeated / sub = planta group = bloco type = AR(1);
run;


proc mixed data = dados;
	class trat planta;
	model diametro = trat avaliacao trat * avaliacao / solution;
	repeated / sub = planta type = cs;
run;
