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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Section
{
	title: qsTr("Inference")

	Group
	{

		CheckBox
		{
			label:		qsTr("Conditional parameter estimates")
			name:		"inferenceConditionalParameterEstimates"
			checked: 	true
		}

		CheckBox
		{
			columns:	2
			label:		qsTr("Models overview")
			name:		"inferenceModelsOverview"

			RadioButtonGroup
			{
				name: "inferenceModelsOverviewBfComparison"
				title: qsTr("BF Comparison")

				RadioButton
				{
					name: 		"inclusion"
					label: 		qsTr("Inclusion")
					checked: 	true
				}

				RadioButton
				{
					name: 		"best"
					label: 		qsTr("vs. best")
				}

				RadioButton
				{
					name: 		"previous"
					label: 		qsTr("vs. previous")
					enabled:	inferenceModelsOverviewOrderMarglik.checked
				}
			}

			RadioButtonGroup
			{
				name: 		"inferenceModelsOverviewOrder"
				title:		qsTr("Order")

				RadioButton
				{
					name: 		"modelNumber"
					label: 		qsTr("Model number")
					checked:	true
				}

				RadioButton
				{
					name: 		"marginalLikelihood"
					label: 		qsTr("Marginal likelihood")
					id:			inferenceModelsOverviewOrderMarglik
				}

				RadioButton
				{
					name: 		"posteriorProbability"
					label: 		qsTr("Posterior probability")

				}
			}
		}

		CheckBox
		{
			label:		qsTr("Individual models")
			name:		"inferenceIndividualModels"

			CheckBox
			{
				label:		qsTr("Single model")
				name:		"inferenceIndividualModelsSingleModel"
				childrenOnSameRow: true
				IntegerField
				{
					name:	"inferenceIndividualModelsSingleModelNumber"
					defaultValue:	1
				}
			}
		}

	}

	Group
	{

		BayesFactorType{}

		CIField
		{
			name: "inferenceCiWidth"
			label: qsTr("CI width")
		}

		CheckBox
		{
			label:		qsTr("Shorten prior names")
			name:		"inferenceShortenPriorName"
		}

		CheckBox
		{
			label:		qsTr("Precision allocation as standard deviation ratio")
			name:		"inferencePrecisionAllocationAsStandardDeviationRatio"
			checked:	true
		}

	}
}