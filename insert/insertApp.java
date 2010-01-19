/**
 * @file   %f
 * @author %U %a
 * @date   %d 
 * Time-stamp: < >
 *
 * Copyright (c) %y %U %a
 *               %o
 *
 * @version 0.1 $Rev$
 * $Id$ 
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import java.util.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.*;

public class %b {
    public static void main (String args[]) {
	JFrame app = new %bFrame("Application");
	app.setSize(250,100);
	app.setVisible(true); // affichage effectif
    }
};

class %bFrame  extends JFrame  {
    %bPanel _contentPanel; // Le contenu de cette frame
    private static final long serialVersionUID = 1L; // pour éviter un warning

    public %bFrame(String title) {
	super(title); // appel du super-constructeur
	_contentPanel = new %bPanel();             // créé le contenu
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);//ce qui arrive à la fermeture 
	// On ajoute le contenu
	getContentPane().add(_contentPanel); 
   }
};

class %bPanel extends JPanel {
    private JLabel _label; 
    private static final long serialVersionUID = 1L;

    public %bPanel() {
	_label = new JLabel("HelloWorld!", JLabel.CENTER);
	setLayout(new BorderLayout()); // Choix de l'organisation du contenu
	add(_label);
    }
};



