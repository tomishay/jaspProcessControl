
// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 									  2.8
import QtQuick.Layouts 						1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form 
{
	usesJaspResults:							true
    columns:									1

        VariablesForm
        {
            preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
            id:										variablesForm

            AvailableVariablesList
            {
                name:								"variablesForm"
            }

            AssignedVariablesList
            {
                id:									variables
                name:								"variables"
                title:								qsTr("Variables")
                allowedColumns:						["scale"]
            }
            AssignedVariablesList
            {
                id:									variables3
                name:								"D"
                title:								qsTr("Defectives/Defects (attributes)")
                allowedColumns:						["scale"]
                singleVariable:						true
            }
            AssignedVariablesList
            {
                id:									variables4
                name:								"total"
                title:								qsTr("Toatal sample (attributes)")
                allowedColumns:						["scale"]
                singleVariable:						true
            }
        }
        Group
        {
            title: qsTr("Control charts for variables")

       Group {
           columns: 									4
        CheckBox {	name: "Xbarchart";		label: qsTr("X bar chart")              }
        CheckBox {	name: "Rchart";		label: qsTr("R chart")                      }
        CheckBox {	name: "ImRchart";		label: qsTr("I-mR chart for variables")				}
        CheckBox {	name: "Schart";		label: qsTr("S chart")                      }
            }

	}

    Group
    {
		title: qsTr("Control charts for attributes")
        Group{
            columns: 									3
            CheckBox {

                name: "Defectivescharts";		label: qsTr("Defectives charts")
                RadioButtonGroup
                {
                    name:	"TypeDefectives";
                    title:	qsTr("Type")
                    RadioButton { value: "npchart";				label: qsTr("np chart");		checked: true					}
                    RadioButton { value: "pchart";				label: qsTr("p chart")											}
                    RadioButton { value: "Laneyprimechart";		label: qsTr("Laney p’ (p-prime) chart")							}
                }

            }
            CheckBox {	name: "Defectscharts";		label: qsTr("Defects charts")
                RadioButtonGroup
                {
                    name:	"TypeDefects";
                    title:	qsTr("Type")
                    RadioButton { value: "cchart";			label: qsTr("c chart");						checked: true	}
                    RadioButton { value: "uchart";			label: qsTr("u chart")										}
                    RadioButton { value: "Laneychart";		label: qsTr("Laney u’ (u-prime) chart")						}
                }
            }
            CheckBox {	name: "ImRchart2";		label: qsTr("I-mR chart for attributes")												}
        }

	}

    Section
    {
        title: qsTr("Special control charts")
        Group
        {
            VariablesForm
            {
                preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
                id:										variablesForm3

                AvailableVariablesList
                {
                    name:								"variablesForm3"
                }

                AssignedVariablesList
                {
                    id:									variables5
                    name:								"measurements"
                    title:								qsTr("Measurements")
                    allowedColumns:						["scale", "nominal", "ordinal"]
                }
            }
            Group{
                      columns: 									2
            CheckBox {	name: "Cumulativechart";				label: qsTr("Cumulative sum chart")									}
            CheckBox {	name: "Exponentialchart";				label: qsTr("Exponentially weighted moving average chart")			}
            CheckBox {	name: "gchart";							label: qsTr("g chart")												}
            CheckBox {	name: "tchart";							label: qsTr("t chart")												}
            }

        }

    }
}
