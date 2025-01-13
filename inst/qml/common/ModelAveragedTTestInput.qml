//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

ColumnLayout
{
	property string testType:	"robust"

	Formula { rhs: "dependent" }

	VariablesForm
	{
		AvailableVariablesList
		{
			name:				"allVariablesList"
		}

		AssignedVariablesList
		{
			name:				"dependent"
			title:				qsTr("Dependent Variable")
			allowedColumns:		["scale"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			name:				"group"
			title:				qsTr("Grouping Variable")
			allowedColumns:		["nominal"]
			singleVariable:		true
		}
	}

	CheckBox
	{
		name:		"priorDistributionPlot"
		label:		qsTr("Prior distribution plots")
	}

	RadioButtonGroup
	{
		name: 		"truncation"
		title:		qsTr("Truncation")
		visible:	testType == "truncated"
		columns:	2

		RadioButton
		{
			name: 		"truncationStandardDeviation"
			label: 		qsTr("Standard deviation (z-score)")
			checked:	true

			RadioButtonGroup
			{
				name: 		"truncationStandardDeviationGrouping"

				RadioButton
				{
					name: 		"perSample"
					label: 		qsTr("Per sample")
					checked:	true

					FormulaField
					{
						label:				qsTr("Sample 1")
						name:				"truncationStandardDeviationSample1"
						value:				"3"
						min:				0
						inclusive:			JASP.None
						fieldWidth: 		40 * preferencesModel.uiScale
					}

					FormulaField
					{
						label:				qsTr("Sample 2")
						name:				"truncationStandardDeviationSample2"
						value:				"3"
						min:				0
						inclusive:			JASP.None
						fieldWidth: 		40 * preferencesModel.uiScale
					}
				}

				RadioButton
				{
					name: 		"acrossSamples"
					label: 		qsTr("Across samples")

					FormulaField
					{
						label:				qsTr("Both samples")
						name:				"truncationStandardDeviationBothSamples"
						value:				"3"
						min:				0
						inclusive:			JASP.None
						fieldWidth: 		40 * preferencesModel.uiScale
					}
				}
			}
		}

		RadioButton
		{
			name: 		"truncationBounds"
			label: 		qsTr("Bounds")

			RadioButtonGroup
			{
				name: 		"truncationBoundsGrouping"

				RadioButton
				{
					name: 		"perSample"
					label: 		qsTr("Per sample")
					checked:	true

					Group
					{
						title:		qsTr("Sample 1")
						columns:	2

						FormulaField
						{
							label:				qsTr("Lower")
							name:				"truncationBoundsSample1Lower"
							id:					truncationBoundsSample1Lower
							value:				"-3"
							max:				truncationBoundsSample1Upper.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
						}

						FormulaField
						{
							label:				qsTr("Upper")
							name:				"truncationBoundsSample1Upper"
							id:					truncationBoundsSample1Upper
							value:				"3"
							min:				truncationBoundsSample1Lower.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
						}
					}
					
					Group
					{
						title:		qsTr("Sample 2")
						columns:	2

						FormulaField
						{
							label:				qsTr("Lower")
							name:				"truncationBoundsSample2Lower"
							id:					truncationBoundsSample2Lower
							value:				"-3"
							max:				truncationBoundsSample2Upper.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
						}

						FormulaField
						{
							label:				qsTr("Upper")
							name:				"truncationBoundsSample2Upper"
							id:					truncationBoundsSample2Upper
							value:				"3"
							min:				truncationBoundsSample2Lower.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
						}
					}
				}

				RadioButton
				{
					name: 		"acrossSamples"
					label: 		qsTr("Across samples")

					Group
					{
						title:		qsTr("Both Samples")
						columns:	2

						FormulaField
						{
							label:				qsTr("Lower")
							name:				"truncationBoundsBothSamplesLower"
							id:					truncationBoundsBothSamplesLower
							value:				"-3"
							max:				truncationBoundsBothSamplesUpper.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
						}

						FormulaField
						{
							label:				qsTr("Upper")
							name:				"truncationBoundsBothSamplesUpper"
							id:					truncationBoundsBothSamplesUpper
							value:				"3"
							min:				truncationBoundsBothSamplesLower.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
						}
					}
				}
			}
		}
	}
}
