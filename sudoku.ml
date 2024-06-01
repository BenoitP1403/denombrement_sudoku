(* TIPE BENOÎT POREAUX MP*1
Code de Benoît

 *)


type partition = {mutable elt: int array; mutable prof: int array; mutable taille: int array};;

let chemin1 = "fusion1.dat";;
let chemin2 = "fusion2.dat";;
let chemin3 = "fusion3.dat";;
let chemin4 = "fusion4.dat";;
let chemin5 = "fusion5.dat";;
let chemin6 = "fusion6.dat";;
let chemin7 = "fusion7.dat";;
let fich_compt = "comptage.dat";;
(*
si on utilise wincaml on est obligés de spécifier le chemin entier (wincaml semble ne pas prendre pour répertoire courant le chemin du fichier .ml
*)

(*Initialise n classes d'équivalences pour l'union find*)
let init n =
   let p = {elt = Array.make n 0; prof = Array.make n 0; taille = Array.make n 1} in
      for k = 0 to n - 1 do
         p.elt.(k) <- k
      done;
      p;;

(*trouve le représentant d'un élément*)
let rec trouver_racine p k =
   let i = p.elt.(k) in
      if i <> k then
         trouver_racine p i
      else k;;

(*renvoie le nombre de classes qu'il reste*)
let nb_classes p =
   let cpt = ref 0 in
      let n = Array.length p.elt in
         for i = 0 to n - 1 do
            if p.elt.(i) = i then
               cpt := !cpt + 1
         done;
         !cpt;;

(*fusionne deux classes, algorithme du cours.
Non optimal, long, mais non dérangeant:
la durée de cette étape reste négligeable devant la durée de la suivante*)
let fusion p i j =
   let k = trouver_racine p i
   and l = trouver_racine p j in
      if k <> l then
         let s = p.taille.(k) + p.taille.(l) in
            p.taille.(k) <- s;
            p.taille.(l) <- s;
            if p.prof.(k) < p.prof.(l) then
               p.elt.(k) <- l
            else
               (p.elt.(l) <- k;
                  if p.prof.(k) = p.prof.(l) then
                     p.prof.(k) <- p.prof.(k) + 1);;

(*utilitaires de lecture/ écriture sur disque*)
let save_to_file_array filename array =
   let oc = open_out_bin filename in
      Marshal.to_channel oc array [];
      close_out oc;;

let load_from_file_array filename =
   let ic = open_in_bin filename in
      let array = (Marshal.from_channel ic: int array) in
         close_in ic;
         array;;

let save_partition_to_file filename partition =
   let oc = open_out_bin filename in
      Marshal.to_channel oc partition [];
      close_out oc;;

let load_partition_from_file filename =
   let ic = open_in_bin filename in
      let partition = (Marshal.from_channel ic: partition) in
         close_in ic;
         partition;;
(*utilitaire de copie de matrice*)
let copy_matrix matrix =
   let rows = Array.length matrix in
      let cols = Array.length matrix.(0) in
         let new_matrix = Array.make_matrix rows cols matrix.(0).(0) in
            for i = 0 to rows - 1 do
               for j = 0 to cols - 1 do
                  new_matrix.(i).(j) <- matrix.(i).(j)
               done;
            done;
            new_matrix;;

(*renvoie 1+log2 m si m puissance de deux, utilisé pour afficher une grille*)
let masq_to_n m =
   match m with
   | 1 -> 1
   | 2 -> 2
   | 4 -> 3
   | 8 -> 4
   | 16 -> 5
   | 32 -> 6
   | 64 -> 7
   | 128 -> 8
   | 256 -> 9
   | _ -> failwith "print_error";;

let swap_m m i j i' j' =
   let tmp = m.(i).(j) in
      m.(i).(j) <- m.(i').(j');
      m.(i').(j') <- tmp;;

(*utilitaire, affiche une matrice*)
let afficher_tableau_tableau (tab: int array array): unit =
   Array.iter (fun sub_tab ->
         Array.iter (fun x -> print_int x; print_string " ") sub_tab;
         print_newline ()
   ) tab;;
let afficher_tableau_tableau_mod (tab: int array array): unit =
   Array.iter (fun sub_tab ->
         Array.iter (fun x -> print_int (masq_to_n x); print_string " ") sub_tab;
         print_newline ()
   ) tab;;


(*Les fonctions suivantes testent si "boites", ie les trois boites du haut, peut mener à une grille valide*)

(*les lignes sont-elles non contradictoires?*)
let ligne_valide boites i =
   let test = Array.make 10 0 in
      for j = 1 to 9 do
         test.(boites.(i).(j)) <- 1 + test.(boites.(i).(j))
      done;
      test.(1) <= 1 && test.(2) <= 1 && test.(3) <= 1 && test.(4) <= 1 && test.(5) <= 1 && test.(6) <= 1 && test.(7) <= 1 && test.(8) <= 1 && test.(9) <= 1;;
let lignes_valides boites =
   (ligne_valide boites 1) && (ligne_valide boites 2) && (ligne_valide boites 3);;




(*idem pour les boites*)
let boite_valide boites i =
   let test = Array.make 10 0 in
      for x = 1 to 3 do
         for y = (i - 1) * 3 + 1 to (i) * 3 do
            test.(boites.(x).(y)) <- test.(boites.(x).(y)) + 1
         done;
      done;
      test.(1) <= 1 && test.(2) <= 1 && test.(3) <= 1 && test.(4) <= 1 && test.(5) <= 1 && test.(6) <= 1 && test.(7) <= 1 && test.(8) <= 1 && test.(9) <= 1;;

let boites_valides boites =
   (boite_valide boites 1) && (boite_valide boites 2) && (boite_valide boites 3);;


(*idem pour les colonnes*)
let colonne_valide boites c =
   (boites.(1).(c) <> boites.(2).(c)
      &&
      boites.(2).(c) <> boites.(3).(c)
      &&
      boites.(3).(c) <> boites.(1).(c)) || boites.(1).(c) = 0 || boites.(2).(c) = 0 || boites.(3).(c) = 0;;

let colonnes_valides boites =
   let res = ref true in
      for c = 4 to 9 do
         res := !res && (colonne_valide boites c)
      done;
      !res;;


let rec linf l1 l2 = match l1, l2 with
   | (_, 0 :: q) -> true
   | (0 :: q, _) -> true
   | ([], []) -> true
   | (t1 :: q1, t2 :: q2) -> (t1 = t2 && (linf q1 q2)) || t1 < t2;;

let print_bool b =
   if b then print_string "TRUE\n" else print_string "FALSE\n";;

(*une règle sur l'ordre des boites*)
let regle_ordre boites =
   let l1 = [boites.(1).(4); boites.(1).(5); boites.(1).(6); boites.(2).(4); boites.(2).(5); boites.(2).(6); boites.(3).(4); boites.(3).(5); boites.(3).(6)]
   and l2 = [boites.(1).(4 + 3); boites.(1).(5 + 3); boites.(1).(6 + 3); boites.(2).(4 + 3); boites.(2).(5 + 3); boites.(2).(6 + 3); boites.(3).(4 + 3); boites.(3).(5 + 3); boites.(3).(6 + 3)] in
      linf l1 l2
      &&
      (boites.(1).(4) <= boites.(1).(5) || boites.(1).(5) = 0)
      &&
      (boites.(1).(5) <= boites.(1).(6) || boites.(1).(6) = 0)
      &&
      (boites.(1).(7) <= boites.(1).(8) || boites.(1).(8) = 0)
      &&
      (boites.(1).(8) <= boites.(1).(9) || boites.(1).(9) = 0)


      let boites_valide boites =
         (lignes_valides boites) && (boites_valides boites) && (regle_ordre boites) && (colonnes_valides boites);;




(*donne la case suivante*)
let suivant l c =
   if c = 9 then (l + 1, 4)
   else (l, c + 1);;

(*affiche cpt tous les 1000*)
let disp cpt = if cpt mod 1000 = 0 then print_int cpt; print_newline ();;


(*génère les quelques 36288 grilles de départ*)
let generate () =
   let cpt = ref 0 in
      let resp = Array.make_matrix 4 10 0 in
         resp.(1).(1) <- 1; resp.(1).(2) <- 2; resp.(1).(3) <- 3; resp.(2).(1) <- 4; resp.(2).(2) <- 5; resp.(2).(3) <- 6; resp.(3).(1) <- 7; resp.(3).(2) <- 8; resp.(3).(3) <- 9;

         let rec aux x y mat =
            match (boites_valide mat, x, y) with
            | (false, _, _) -> []
            | (true, 4, 4) -> incr cpt; print_int !cpt; print_newline ();
               afficher_tableau_tableau mat; print_newline ();
               [copy_matrix mat];
            | (true, _, _) -> let res = ref [] in
                  for i = 1 to 9 do
                     mat.(x).(y) <- i;
                     let (x', y') = suivant x y in
                        res := !res @ (aux x' y' (mat));
                        mat.(x).(y) <- 0;
                  done;
                  !res;
         in aux 1 4 resp;;


let res1 = generate ();;
print_string "resultat1:\n";;
print_int (List.length (res1));;


let dep = Array.of_list res1;;
let n = Array.length dep;;
let uf = init n;;

(*On agit sur les classes pour les fusionner*)
let echanger_lignes boites i1 i2 =
   for j = 1 to 9 do
      let tmp = boites.(i1).(j) in
         boites.(i1).(j) <- boites.(i2).(j);
         boites.(i2).(j) <- tmp;
   done;;
let echanger_colonnes boites c1 c2 =
   for i = 1 to 3 do
      let tmp = boites.(i).(c1) in
         boites.(i).(c1) <- boites.(i).(c2);
         boites.(i).(c2) <- tmp;
   done;;
let echangerb boites x y =
   for i = 1 to 3 do
      for j = 1 to 3 do
         swap_m boites i (j + (y - 1) * 3) i (j + (x - 1) * 3)
      done
   done;;
(*écchange les boites deux et trois*)
let echangerb23 boites =
   echangerb boites 2 3;;
(*trieles colonnes*)
let tri_colonnes boites =
   if (boites.(1).(4) > boites.(1).(5)) then
      echanger_colonnes boites 4 5;
   if (boites.(1).(5) > boites.(1).(6)) then
      echanger_colonnes boites 5 6;
   if (boites.(1).(4) > boites.(1).(5)) then
      echanger_colonnes boites 4 5;

   if (boites.(1).(7) > boites.(1).(8)) then
      echanger_colonnes boites 7 8;
   if (boites.(1).(8) > boites.(1).(9)) then
      echanger_colonnes boites 8 9;
   if (boites.(1).(7) > boites.(1).(8)) then
      echanger_colonnes boites 7 8;;
(*normalise boites (ie transforme la boite 1 en 123456789 ) *)
let normaliser_chiffres boites =
   let v1 = boites.(1).(1) and
   v2 = boites.(1).(2) and
   v3 = boites.(1).(3) and
   v4 = boites.(2).(1) and
   v5 = boites.(2).(2) and
   v6 = boites.(2).(3) and
   v7 = boites.(3).(1) and
   v8 = boites.(3).(2) and
   v9 = boites.(3).(3) in
      for i = 1 to 3 do
         for j = 1 to 9 do
            match boites.(i).(j) with
            | k when (k = v1) -> (boites.(i).(j) <- 1)
            | k when (k = v2) -> (boites.(i).(j) <- 2)
            | k when (k = v3) -> (boites.(i).(j) <- 3)
            | k when (k = v4) -> (boites.(i).(j) <- 4)
            | k when (k = v5) -> (boites.(i).(j) <- 5)
            | k when (k = v6) -> (boites.(i).(j) <- 6)
            | k when (k = v7) -> (boites.(i).(j) <- 7)
            | k when (k = v8) -> (boites.(i).(j) <- 8)
            | k when (k = v9) -> (boites.(i).(j) <- 9)
         done;
      done;;

(*on normalise les chiffres et on trie les boites deux et trois*)
let normaliser boites =
   normaliser_chiffres boites;
   tri_colonnes boites;
   let l1 = [boites.(1).(4); boites.(1).(5); boites.(1).(6); boites.(2).(4); boites.(2).(5); boites.(2).(6); boites.(3).(4); boites.(3).(5); boites.(3).(6)]
   and l2 = [boites.(1).(4 + 3); boites.(1).(5 + 3); boites.(1).(6 + 3); boites.(2).(4 + 3); boites.(2).(5 + 3); boites.(2).(6 + 3); boites.(3).(4 + 3); boites.(3).(5 + 3); boites.(3).(6 + 3)] in
      if not (linf l1 l2) then
         echangerb23 boites;
;;

(*cherche boites dans les matrices de départ. Vu la construction, on doit trouver quelque chose*)
let indice_boites boites =
   let r = ref (- 1) in
      for i = 0 to n - 1 do
         if dep.(i) = boites
         then (r := i;)
      done;
      !r;;


(*lie boites normalisé à la classe d'indice i*)
let lier_b boites i =
   normaliser boites;
   let n' = indice_boites boites in
      match n' with
      | (- 1) -> failwith "error"
      | _ -> fusion uf n' i;;



(*fait des échanges de lignes, normalise et lie*)
let fusionne1 () =
   for i = 0 to n - 1 do
      print_string "fusionne1 etape :"; print_int i; print_newline ();
      let b' = copy_matrix dep.(i) in
         echanger_lignes b' 1 2;
         lier_b b' i;
         echanger_lignes b' 2 1;
         echanger_lignes b' 1 3;
         lier_b b' i;
         echanger_lignes b' 3 1;
   done;;

(*regarde sur disque si le calcul n'a pas déjà été effectué*)
let exec1 () =
   if Sys.file_exists chemin1 then
      let stor = load_partition_from_file chemin1 in
         uf.elt <- stor.elt;
         uf.prof <- stor.prof;
         uf.taille <- stor.taille;
   else
      fusionne1 ();;
exec1();;
print_string "res fusionne1: ";;
print_int (nb_classes(uf));;
(*on écrit sur disque le résultat qui vient d'être calculé*)
save_partition_to_file chemin1 uf;;

(* FUSIONNE1 IS A SUCCESS ON TESTS!  Plus que 6240 classes. Exécution longue (mauvaise optimisation??)*)
(*On pourrait finir en backtracking*)


(*même principe pour fusionne2 et fusionne3*)
let fusionne2 () =
   for i = 0 to n - 1 do
      print_string "fusionne2 etape :"; print_int i; print_newline ();
      let b' = copy_matrix dep.(i) in
         echanger_colonnes b' 1 2;
         lier_b b' i;
         echanger_colonnes b' 2 1;
         echanger_colonnes b' 1 3;
         lier_b b' i;
         echanger_colonnes b' 3 1;
   done;;

let exec2 () =
   if Sys.file_exists chemin2 then
      let stor = load_partition_from_file chemin2 in
         uf.elt <- stor.elt;
         uf.prof <- stor.prof;
         uf.taille <- stor.taille;
   else
      fusionne2 ();;
exec2();;
print_string "res fusionne2: ";;
print_int (nb_classes(uf));;

save_partition_to_file chemin2 uf;;


(* FUSIONNE2 IS A SUCCESS! Plus que 1089 classes*)


let fusionne3 () =
   for i = 0 to n - 1 do
      print_string "fusionne3 etape :"; print_int i; print_newline ();
      let b' = copy_matrix dep.(i) in
         echangerb b' 1 2;
         lier_b b' i;
         echangerb b' 2 1;
         echangerb b' 1 3;
         lier_b b' i;
         echangerb b' 3 1;
   done;;

let exec3 () =
   if Sys.file_exists chemin3 then
      let stor = load_partition_from_file chemin3 in
         uf.elt <- stor.elt;
         uf.prof <- stor.prof;
         uf.taille <- stor.taille;
   else
      fusionne3 ();;
exec3();;
print_string "res fusionne3: ";;
print_int (nb_classes(uf));;

save_partition_to_file chemin3 uf;;
(*plus que 416 classes*)

(*fusionne 4*)
let fusionne4 () =
  for i = 0 to n - 1 do
    print_string "fusionne4 etape :"; print_int i; print_newline ();
     let b = copy_matrix dep.(i) in
     for x1=1 to 8 do
       for y1=1 to 2 do
         for x2=x1+1 to 9 do
           for y2 = y1 + 1 to 3 do
             if b.(y1).(x1) = b.(y2).(x2) &&
                  b.(y1).(x2) = b.(y2).(x1) then begin
    let b' = copy_matrix b in
    swap_m b' y1 x1 y1 x2;
    swap_m b' y2 x1 y2 x2;
    lier_b b' i
  end
           done
         done
       done
     done
   done;;

let exec4 () =
   if Sys.file_exists chemin4 then
      let stor = load_partition_from_file chemin4 in
         uf.elt <- stor.elt;
         uf.prof <- stor.prof;
         uf.taille <- stor.taille;
   else
      fusionne4 ();;
exec4();;
print_string "res fusionne4: ";;
print_int (nb_classes(uf));;
print_newline ();;

save_partition_to_file chemin4 uf;;
(*174 config ici*)
(* étape 5*)
let fusionne5 () =
   for i = 0 to n - 1 do
      print_string "fusionne5 étape :"; print_int i; print_newline ();
      let b = copy_matrix dep.(i) in
      for x1=1 to 8 do
        for x2=x1+1 to 9 do
          if b.(1).(x1) = b.(2).(x2)
             && b.(2).(x1) = b.(3).(x2)
             && b.(3).(x1) = b.(1).(x2) then
            let b' = copy_matrix b in
            echanger_colonnes b' x1 x2;
            lier_b b' i            
        done
      done
   done;;

let exec5 () =
   if Sys.file_exists chemin5 then
      let stor = load_partition_from_file chemin5 in
         uf.elt <- stor.elt;
         uf.prof <- stor.prof;
         uf.taille <- stor.taille;
   else
      fusionne5 ();;
exec5();;
print_string "res fusionne5: ";;
print_int (nb_classes(uf));;

save_partition_to_file chemin5 uf;;
(*plus que 141 classes*)

(* étape 6*)
let fusionne6 () =
   for i = 0 to n - 1 do
      print_string "fusionne6 étape :"; print_int i; print_newline ();
      let b = copy_matrix dep.(i) in
      for x1=1 to 3 do
        for x2=3 to 6 do
          for x3= 7 to 9 do
            for y1 =1 to 2 do
              for y2 = y1+1 to 3 do
                if  b.(y1).(x1) = b.(y2).(x2)
                    && b.(y1).(x2) = b.(y2).(x3)
                    && b.(y1).(x3) = b.(y2).(x1) then
                  let b' = copy_matrix b in
                  swap_m b' y1 x1 y2 x1;
                  swap_m b' y1 x2 y2 x2;
                  swap_m b' y1 x3 y2 x3;
                  lier_b b' i
              done
            done
          done
        done
      done
      
  
   done;;

let exec6 () =
   if Sys.file_exists chemin6 then
      let stor = load_partition_from_file chemin6 in
         uf.elt <- stor.elt;
         uf.prof <- stor.prof;
         uf.taille <- stor.taille;
   else
      fusionne6 ();;
exec6();;
print_string "res fusionne6: ";;
print_int (nb_classes(uf));;
print_newline ();;

save_partition_to_file chemin6 uf;;
(*plus que 86 classes*)


(*fusionne 7*)
let fusionne7 () =
  for i = 0 to n - 1 do
    print_string "fusionne7 etape :"; print_int i; print_newline ();
    let b = copy_matrix dep.(i) in
    for x1 = 1 to 6 do
      for x2 = x1 + 1 to 7 do
        for x3 = x2 + 1 to 8 do
          for x4 = x3 + 1 to 9 do
            for y1 = 1 to 2 do
              for y2 = y1 + 1 to 3 do
                if    b.(y1).(x1)=b.(y2).(x3)
                   && b.(y1).(x3) = b.(y2).(x2)
                   && b.(y1).(x2) = b.(y2).(x4)
                   && b.(y1).(x4) = b.(y2).(x1)
                   || b.(y1).(x1) = b.(y2).(x3)
                   && b.(y1).(x3) = b.(y2).(x4)
                   && b.(y1).(x4) = b.(y2).(x3)
                   && b.(y1).(x3) = b.(y2).(x1) then
                  let b' = copy_matrix b in
                  swap_m b' y1 x1 y2 x1;
                  swap_m b' y1 x2 y2 x2;
                  swap_m b' y1 x3 y2 x3;
                  swap_m b' y1 x4 y2 x4;
                  lier_b b' i;
              done
            done
          done
        done
      done
    done
  
   done;;

let exec7 () =
   if Sys.file_exists chemin7 then
      let stor = load_partition_from_file chemin7 in
         uf.elt <- stor.elt;
         uf.prof <- stor.prof;
         uf.taille <- stor.taille;
   else
      fusionne7 ();;
exec7();;
print_string "res fusionne7: ";;
print_int (nb_classes(uf));;
print_newline ();;

save_partition_to_file chemin7 uf;;
(*71 config ici*)


(*On a fini de regrouper les classes, on les synthétise*)
let cree_repr () =
   let nb = nb_classes (uf) in
   let representants = Array.make nb (dep.(0), 0) in
   let cpt = ref 0 in
   let somme = ref 0 in
   for i = 0 to n - 1 do
      if uf.elt.(i) = i then
      begin representants.(!cpt) <- (dep.(i), uf.taille.(i)); (*print_int uf.taille.(i);print_string "\n"; *) somme := !somme + uf.taille.(i); incr cpt;
      end
   done;
   print_int !somme;
   representants;;
let representants = cree_repr();;



(*l'ordre du parcours  de backtracking, conseillé dans l'article de Jarvis*)
let ordre = [|
      (9, 2); (8, 2); (7, 2); (6, 2); (5, 2); (4, 2);
      (9, 3); (8, 3); (7, 3); (6, 3); (5, 3); (4, 3);
      (9, 4); (8, 4); (7, 4); (6, 4); (5, 4); (4, 4);
      (4, 5); (4, 6); (4, 7); (4, 8); (4, 9);
      (9, 5); (8, 5); (7, 5); (6, 5); (5, 5);
      (5, 6); (5, 7); (5, 8); (5, 9);
      (9, 6); (8, 6); (7, 6); (6, 6);
      (6, 7); (6, 8); (6, 9);
      (9, 7); (8, 7); (7, 7);
      (7, 8); (7, 9);
      (9, 8); (8, 8);
      (8, 9);
      (9, 9)|];;



(*let test =fst representants.(0);;*)


(*let u = Array.make_matrix 3 9 0;;
let tableau = Array.make_matrix 9 9 0;;
 afficher_tableau_tableau test;;
*)


(*Le tableau u est de la forme:

511 511 511 511 511 511 511 511 511 
511 511 511 511 511 511 511 511 511 
511 511 511 511 511 511 511 511 511 
et contient les masques des possibilités déjà utilisées pour chaque ligne/ colonne/case
première ligne: pour les lignes
deuxième ligne: pour les colonnes
troisième ligne: pour les boites

tableau représente le tableau, et n'est pas essentiel sauf pour de l'affichage
*)

(*on place un nombre de masque ma en position x, y*)
let placer u tableau x y ma =
   u.(0).(x) <- u.(0).(x) + ma;
   u.(1).(y) <- u.(1).(y) + ma;
   u.(2).((x / 3) * 3 + (y / 3)) <- u.(2).((x / 3) * 3 + (y / 3)) + ma;
   tableau.(x).(y) <- ma;;
(*on le retire*)
let retirer u tableau x y ma =
   u.(0).(x) <- u.(0).(x) - ma;
   u.(1).(y) <- u.(1).(y) - ma;
   u.(2).((x / 3) * 3 + (y / 3)) <- u.(2).((x / 3) * 3 + (y / 3)) - ma;
   tableau.(x).(y) <- 0;;


(*on remplir u et tableau avec les données initiales*)
let remplir_init u tableau mat =
   for i = 0 to 2 do
      for j = 0 to 8 do
         let n = mat.(i + 1).(j + 1) in
         if n <> 0 then
            placer u tableau i j (1 lsl (n - 1))
      done
   done
;;


(*remplir_init u tableau test;;
test;;

placer u tableau 3 0 (1 lsl (2-1));;
placer u tableau 4 0 (1 lsl (3-1));;
placer u tableau 5 0 (1 lsl (5-1));;
placer u tableau 6 0 (1 lsl (6-1));;
placer u tableau 7 0 (1 lsl (8-1));;
placer u tableau 8 0 (1 lsl (9-1));;
afficher_tableau_tableau tableau;;
afficher_tableau_tableau u;;
*)

(*renvoie un masque qui code les différentes possibilités en x, y*)
let masque u x y =
   u.(0).(x) lor u.(1).(y) lor u.(2).((x / 3) * 3 + (y / 3));;
let cpt = ref 0;;
(*affichage, deprecated*)
let affichage u tableau n =
   if n mod 100000 = 0 then
      (print_int !cpt;
         print_newline ();
         afficher_tableau_tableau u;
         afficher_tableau_tableau_mod tableau;
      );;

(*fonction de backtracking optimisé, utilisant ordre*)
let rec parcours u tableau num =
   let res = ref 0 in
   if num >= Array.length ordre then
      begin 1;
      end
   else
      begin
      let x, y = ordre.(num) in
      let m = ref ((masque u (x - 1) (y - 1)) lxor 0b111111111) in
         while !m <> 0 do
            if num = 0 then
               begin print_string "On est au premier appel, indice"; print_int !m; print_newline (); end;
            let i = !m land (- !m) in (*le plus petit bit à 1*)
            m := !m - i;
            placer u tableau (x - 1) (y - 1) i;
            res := !res + parcours u tableau (num + 1);
            retirer u tableau (x - 1) (y - 1) i;
         done;
         !res;
      end;
;;

(*parcours u tableau 0;;*)

(*on compte toutes les possibilités pour le représentnat repres dont la première colonne est complétée par col1*)
let comptage repres col1 =
   let u = Array.make_matrix 3 9 0 in
   let tableau = Array.make_matrix 9 9 0 in
      remplir_init u tableau repres;
      placer u tableau 3 0 (1 lsl (col1.(0) - 1));
      placer u tableau 4 0 (1 lsl (col1.(1) - 1));
      placer u tableau 5 0 (1 lsl (col1.(2) - 1));
      placer u tableau 6 0 (1 lsl (col1.(3) - 1));
      placer u tableau 7 0 (1 lsl (col1.(4) - 1));
      placer u tableau 8 0 (1 lsl (col1.(5) - 1));
      parcours u tableau 0;;

(*
Pour la première colonne:
2 3 et 5 triés;
6 8 et 9 ttriés
2 et 6 triés

2;3;5;6;8;9
2;3;6;5;8;9
2;3;8;5;6;9
2;3;9;5;6;8
2;5;6;3;8;9
2;5;8;3;6;9
2;5;9;3;6;8
2;6;8;3;5;9
2;6;9;3;5;8
2;8;9;3;5;6
*)


(*On se ramène à ses valeurs pour col1, quitte à devoir multiplier par 72 ensuite*)
let poss_col_1 = [|
      [|2; 3; 5; 6; 8; 9|];
      [|2; 3; 6; 5; 8; 9|];
      [|2; 3; 8; 5; 6; 9|];
      [|2; 3; 9; 5; 6; 8|];
      [|2; 5; 6; 3; 8; 9|];
      [|2; 5; 8; 3; 6; 9|];
      [|2; 5; 9; 3; 6; 8|];
      [|2; 6; 8; 3; 5; 9|];
      [|2; 6; 9; 3; 5; 8|];
      [|2; 8; 9; 3; 5; 6|]
   |];;

(*Pour un reorésentant, on calcule le nombre de grilles. Long*)
let denomb repres =
   let res = ref 0 in
      for i = 0 to (Array.length poss_col_1) - 1 do
         res := !res + comptage repres poss_col_1.(i)
      done;
      !res;;
(*
denomb (fst representants.(0) );;
renvoie 108374976
success! correspond aux données fournies par Jarvis :) :) :) *)

(*On somme sur chaque représentant en pondérant par la taille de chaque classe.
On obtient le résultat, qu'il faut ensuite multiplier par 9!*72^2.
On ne le fait pas en caml pour éviter un overflow
 *)
let compter_tot () =
  let t =Array.length representants in
  print_int  t;
  let sto_res = Array.make t (-1) in
  if Sys.file_exists fich_compt then
    begin
    print_endline "On charge la sauvegarde";
    let tab = load_from_file_array fich_compt in
    for i = 0 to (Array.length sto_res)-1 do
      sto_res.(i)<- tab.(i)
    done;
    end;
   let res = ref 0 in
      for i = 0 to - 1 + Array.length representants do
        let repres, mult = representants.(i) in
        if sto_res.(i)= -1 then
          begin
            sto_res.(i)<- denomb repres;
            save_to_file_array fich_compt sto_res
          end;
        print_endline "étape achevée numéro";
        print_int i;
        print_string " ";
        print_int sto_res.(i);
        print_endline " multiplicité ";
        print_int mult;
        print_newline();
        res := !res + mult * (sto_res.(i));
      done;
      print_string "resultat final : 9!*72^2 *"; print_int !res; print_newline ();;
compter_tot() ;; (*très long...*)
(*renvoie 6670903752021072936960 , trop grand pour être représenté en caml ( donc on ne le calcule pas ) *)
