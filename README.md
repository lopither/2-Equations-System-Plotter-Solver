# 🧮 System of Equations Plotter-Solver (Wolfram Language)

This notebook allows you to:
- Input **two equations** in `x` and `y`
- Compute their **intersection points**
- Display both equations and intersections on a clean, labeled plot
- Show the **y-axis intersections** of the equations (so you always see the full curves)
- Automatically adjust the plot range so all relevant points are visible
- Display everything nicely formatted for **dark mode**

---

## ⚙️ Features
✅ Works with **linear** and **nonlinear** equations  
✅ Handles **vertical lines** like `x == 3`  
✅ Automatically zooms to fit all curves and intersections  
✅ Plots labeled intersection points **A, B, ...**  
✅ Shows equation texts and intersection coordinates **next to the plot**  
✅ Uses **dark theme–friendly white text**

---

## 💻 Full Code

```wolfram
(* Helper to safely parse user input *)
safeParse[input_] := Quiet@Check[ToExpression[input, StandardForm, Hold], $Failed]

(* Prompt for two equations *)
eq1Input = InputString["Enter the first equation in x and y (e.g., y == 2 x + 1):"];
eq2Input = InputString["Enter the second equation in x and y (e.g., y == -x + 3):"];

heldEq1 = safeParse[eq1Input];
heldEq2 = safeParse[eq2Input];

(* Validate both are equations *)
If[! MatchQ[heldEq1, Hold[_Equal]] || ! MatchQ[heldEq2, Hold[_Equal]],
  Print["Please enter valid equations using '=='. Example: y == 2 x + 1"],
  
  (* Release Hold to evaluate *)
  eq1 = ReleaseHold[heldEq1];
  eq2 = ReleaseHold[heldEq2];
  
  (* Solve system *)
  intersection = Quiet@Solve[{eq1, eq2}, {x, y}, Reals];
  points = N[{x, y} /. intersection]; (* numerical coordinates *)
  labels = {"A", "B", "C", "D"}[[;; Length[points]]];
  
  If[points === {} || ! VectorQ[Flatten[points], NumericQ],
    Print["No real intersections found."],
    
    (* Attempt to find y-axis intersections (x=0) *)
    yInt1 = Quiet@Solve[eq1 /. x -> 0, y, Reals];
    yInt2 = Quiet@Solve[eq2 /. x -> 0, y, Reals];
    yIntVals = DeleteCases[Flatten[{y /. yInt1, y /. yInt2}], _Solve];
    yIntVals = Select[yIntVals, NumericQ];
    
    (* Determine plot range dynamically: includes intersections and y-axis crosses *)
    allX = Join[points[[All, 1]], {0}];
    allY = Join[points[[All, 2]], yIntVals];
    xMin = Min[-10, Min[allX] - 5];
    xMax = Max[10, Max[allX] + 5];
    yMin = Min[-10, Min[allY] - 5];
    yMax = Max[10, Max[allY] + 5];
    
    xRange = {xMin, xMax};
    yRange = {yMin, yMax};
    
    (* Detect vertical lines and create plotting functions accordingly *)
    toPlotGraphics[eq_, color_] := Module[{solY, solX},
      solY = Quiet[Solve[eq, y]];
      solX = Quiet[Solve[eq, x]];
      Which[
        solY =!= {} && FreeQ[solY, Solve], 
          Plot[y /. solY[[1]], {x, xRange[[1]], xRange[[2]]},
            PlotStyle -> color, PlotRange -> yRange],
        solX =!= {} && FreeQ[solX, Solve], 
          Graphics[{color, Thick, 
            Line[{{x /. solX[[1]], yRange[[1]]}, {x /. solX[[1]], yRange[[2]]}}]}],
        True, Graphics[{}]
      ]
    ];
    
    curve1 = toPlotGraphics[eq1, Blue];
    curve2 = toPlotGraphics[eq2, Red];
    
    (* Mark and label intersections with A, B, ... *)
    markers = Graphics[{
        Red, PointSize[Large], Point[points],
        Table[
          Text[
            Style[labels[[i]], 13, White, Bold, Background -> Black],
            points[[i]] + {0.5, 0.5}
          ],
          {i, Length[points]}
        ]
      }];
    
    (* Combine visuals *)
    plot = Show[
      curve1, curve2, markers,
      PlotRange -> {{xMin, xMax}, {yMin, yMax}},
      AxesLabel -> {"x", "y"},
      PlotLabel -> "System of Equations and Intersections"
    ];
    
    (* Create readable coordinate text *)
    coordText = StringJoin[
      Riffle[
        Table[
          labels[[i]] <> " = " <> ToString[N[points[[i]], 3], InputForm],
          {i, Length[points]}
        ],
        ",  "
      ]
    ];
    
    (* Display everything side by side *)
    Grid[
      {
        {plot, 
         Column[{
           Style["Equation 1 (blue): " <> ToString[eq1, InputForm], 14, Blue, Bold],
           Style["Equation 2 (red): " <> ToString[eq2, InputForm], 14, Red, Bold],
           Spacer[10],
           Style["Intersections:", 14, White, Bold],
           Style[coordText, 13, White, Bold]
         }, Spacings -> 1.5]
        }
      },
      Spacings -> {2, 1}
    ]
  ]
]
