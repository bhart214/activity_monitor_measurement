# activity_monitor_measurement
Study assessing the agreement between various activity monitors on patients with Parkinson's Disease

We looked at two different techniques to measure the agreement between measurements: Bland-Altman plots and Intraclass Correlation.

Bland-Altman plots involve visual inspection and can be used to assess sample size.
ICC is closely related to linear random effects models.

https://docs.google.com/presentation/d/1FkYwcmRhYR6gsKHCDc73iBws9cWYdZPVyMZjgF1RaN0/edit?usp=sharing

Our interest lies in how the monitor recorded steps differ from the actual steps recorded.  If we keep the response variable as "steps" generally (combining both actual and recorded), then we can build our model with device and subject as a random effect.  This allows the variance outputs to tell us HOW MUCH variability there is between the "steps" response when going from "device 1 (monitor)" to "device 2 (true)," which is exactly what we want to know.  If we use the difference as our response variable, meaning we drop the random effect of "device," this only tells us how much variation there is in between subjects (and tasks if we wanted to add that to the model), which is of interest but not answering our question.   So we want to have the basic model of 

 Steps ~ Device(random) + Subject(random) 

for each of the 4 possible tasks (2 min walk slow, 2 min walk fast, home obstacle, community obstacle) and also left vs. right, which will point us to how much variation there is between true and recorded due to subjects and due to device (hoping that variation due to device is fairly low and variation due to subject is high, giving us a higher ICC).  There is more that we could do here in building more complicated models, but keep in mind what the clients want (something they understand and can produce easily) and that we are winding down the semester.
