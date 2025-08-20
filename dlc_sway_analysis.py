# Relevant information:

#1) The code is based on the deeplabcut output files. It analyzes the lateral body movement of the center of mass of the animal.
#2) Python 3.11.5 was used to run the code.
#3) Create a folder and put each animal deeplabcut csv file and video (.mov version, 1080p resolution) in it. 
#4) Call the folder "test" and put it on your desktop. If you want to change the name of the folder or the location, you can do it in the code below.
#5) It analyzes up to two groups, here called "control" and "mutant". You can modify the nomenclature according to your needs.
#6) The code will generate the following documents:
    #A) A pdf file with the plots of the lateral body movement of each animal and a pdf file with the plots of the group analysis.
    #B) An excel file with the data of the lateral body movement of each animal and a pdf file with the group analysis data.
    #C) A video with the lateral body movement of each animal.
    #D) A pdf file with a table with the data of the lateral body movement of each animal.


import os
import re
import cv2
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.signal import welch
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.gridspec import GridSpec
from io import BytesIO
from scipy.stats import ttest_ind  # Import for the t-test
import logging
from fpdf import FPDF  # Used for creating the PDF table

# Set matplotlib to use the "Agg" backend
plt.switch_backend('Agg')

# Constants that can be adjusted as needed
LIKELIHOOD_THRESHOLD = 0.90
FRAME_RATE = 9.7
PLOT_INTERVAL = 25
TRAIL_LENGTH = 50
EXTEND_FACTOR = 1.5
INTERPOLATE_FACTOR = 0.3

def load_csv_r_style(file_path):
    """Loads CSV data, assigning appropriate headers."""
    data = pd.read_csv(file_path, skiprows=1, header=None)
    header_combined = data.iloc[0].astype(str) + "." + data.iloc[1].astype(str)
    data_with_new_header = data[2:].copy()
    data_with_new_header.columns = header_combined
    data_with_new_header.reset_index(drop=True, inplace=True)

    # Convert relevant columns to float
    float_cols = [col for col in data_with_new_header.columns if any(metric in col for metric in ['x', 'y', 'likelihood'])]
    data_with_new_header[float_cols] = data_with_new_header[float_cols].astype(float)

    return data_with_new_header

def clean_tracking_data(df, likelihood_threshold=LIKELIHOOD_THRESHOLD):
    """Cleans tracking data based on a likelihood threshold."""
    body_parts = ['neck', 'body', 'tailbase']
    for part in body_parts:
        x_col = f"{part}.x"
        y_col = f"{part}.y"
        likelihood_col = f"{part}.likelihood"
        mask = df[likelihood_col] < likelihood_threshold
        df.loc[mask, x_col] = np.nan
        df.loc[mask, y_col] = np.nan

    df.interpolate(method='linear', limit_direction='forward', inplace=True)
    return df

def calculate_lateral_sway(neck, body, tailbase):
    """Calculates the lateral sway based on body part positions."""
    long_axis = np.array([tailbase[0] - neck[0], tailbase[1] - neck[1]])
    perpendicular = np.array([-long_axis[1], long_axis[0]])
    perpendicular_normalized = perpendicular / np.linalg.norm(perpendicular)
    neck_to_CoM = np.array([body[0] - neck[0], body[1] - neck[1]])
    projection = np.dot(neck_to_CoM, perpendicular_normalized) * perpendicular_normalized
    return np.linalg.norm(projection)

def calculate_CoM_and_sway_for_selected_parts(dataframe):
    """Calculates the Center of Mass (CoM) and sway for selected body parts."""
    x_cols = ['neck.x', 'body.x', 'tailbase.x']
    y_cols = ['neck.y', 'body.y', 'tailbase.y']
    CoM_x = dataframe[x_cols].mean(axis=1)
    CoM_y = dataframe[y_cols].mean(axis=1)
    sway_magnitude = np.sqrt((CoM_x - CoM_x.mean())**2 + (CoM_y - CoM_y.mean())**2)
    lateral_sway = dataframe.apply(lambda row: calculate_lateral_sway((row['neck.x'], row['neck.y']), 
                                                                     (row['body.x'], row['body.y']), 
                                                                     (row['tailbase.x'], row['tailbase.y'])), axis=1)
    return CoM_x, CoM_y, sway_magnitude, lateral_sway

def update_lateral_sway_plot(ax, lateral_sway, movements, last_frame, annotation_style):
    if lateral_sway.empty:
        print("Warning: No data in lateral_sway for intervals when animal is standing. Skipping plot update for those sections.")
        return ax

    valid_last_frame = min(last_frame, len(lateral_sway))
    valid_movements = np.array(movements)[:valid_last_frame]

    sway_moving = lateral_sway.iloc[:valid_last_frame][valid_movements]
    mean_moving, std_moving = sway_moving.mean(), sway_moving.std()

    ax.clear()
    ax.plot(sway_moving.index, sway_moving, 'r', label='Walking')


    ax.axhline(y=mean_moving, color='r', linestyle='--')
    ax.fill_between(sway_moving.index, mean_moving - std_moving, mean_moving + std_moving, color='r', alpha=0.1)

# Define style parameters for annotations based on the output medium
    if annotation_style == "video":
        y_annotation_height = 1.08
        y_annotation_spacing = 0.02
    elif annotation_style == "pdf":
        y_annotation_height = 1.3  # or any suitable value for PDF
        y_annotation_spacing = 0.08  # or any suitable value for PDF

# Annotations with adjusted positions based on the chosen style
    ax.text(0.98, y_annotation_height - 2 * y_annotation_spacing, f"Mean Walking (dashed red line): {mean_moving:.2f}", transform=ax.transAxes, color='k', ha='right')
    ax.text(0.98, y_annotation_height - 3 * y_annotation_spacing, f"Std. Dev. Walking (pink shadow): {std_moving:.2f}", transform=ax.transAxes, color='k', ha='right')

    # ax.set_title('Lateral Sway Over Time')
    ax.set_xlabel('Time (frames)')
    ax.set_ylabel('Lateral Sway (pixels)')
    ax.legend()

    # Set the y-axis height
    y_min = 0  # Replace with your desired minimum value
    y_max = 60 # Replace with your desired maximum value
    ax.set_ylim(y_min, y_max)


    return ax

def process_standing_and_walking(file_path, pdf_pages, plot_positions):
    """Processes each file, separates standing and walking data, and plots sway and frequency analysis data."""
    data = load_csv_r_style(file_path)
    cleaned_data = clean_tracking_data(data)
    sway_magnitude, movements = calculate_CoM_and_sway_for_selected_parts(cleaned_data)
    indices = range(len(sway_magnitude))

    animal_type = "Unknown"
    animal_number = ""
    match = re.search(r'(control|mutant)\s*(\d+)', file_path.lower())
    if match:
        animal_type, animal_number = match.groups()

# Separate standing and walking/running data
    walking_indices = [i for i, is_standing in enumerate(movements) if is_standing]

# Create PDF for walking/running data
    if walking_indices:
        # Check if a new page should be started
        if plot_positions['count'] % 6 == 0:
            if plot_positions['count'] > 0:
                # Save the current figure to the PDF and close it
                pdf_pages.savefig(plot_positions['fig'])
                plt.close(plot_positions['fig'])

            # Start a new figure with a smaller size to accommodate 6 plots
            plot_positions['fig'], plot_positions['gs'] = plt.subplots(3, 2, figsize=(8.27, 11.69)) # A4 size
            plot_positions['current_plot'] = 0

        # Determine the position of the next plot
        row = plot_positions['current_plot'] // 2
        col = plot_positions['current_plot'] % 2

        # Sway Magnitude Plot for walking/running data
        ax1 = plot_positions['fig'].add_subplot(plot_positions['gs'][row, col])
        ax1.plot([indices[i] for i in walking_indices], [sway_magnitude[i] for i in walking_indices], label='Postural Sway Magnitude')
        ax1.set_title(f'Walking/Running - Postural Sway Magnitude ({animal_type} {animal_number})')
        ax1.set_xlabel('Frame')
        ax1.set_ylabel('Sway Magnitude (units)')
        ax1.legend()
        ax1.grid(True)

        plot_positions['current_plot'] += 1
        plot_positions['count'] += 2  # Two plots have been added

        # If we reached the end of a page, save and reset the count for the current figure
        if plot_positions['current_plot'] >= 6:
            pdf_pages.savefig(plot_positions['fig'])
            plt.close(plot_positions['fig'])
            plot_positions['current_plot'] = 0  # Reset for the next page

def concatenate_video_and_CoM_sway_from_df(video_path, cleaned_data, output_video_path, plot_pdf_path_combined):  # added parameter here
    # Set up the video capture and writer
    cap = cv2.VideoCapture(video_path)
    fourcc = cv2.VideoWriter_fourcc(*'avc1')
    video_width = int(cap.get(3))
    video_height = int(cap.get(4))
    out = cv2.VideoWriter(output_video_path, fourcc, FRAME_RATE, (video_width * 2, video_height))
    out.set(cv2.VIDEOWRITER_PROP_QUALITY, 90)

    # Initialize the frame count and retrieve sway data
    frame_count = 0
    CoM_x, CoM_y, _, lateral_sway = calculate_CoM_and_sway_for_selected_parts(cleaned_data)

    # Check if lateral_sway is empty and handle it
    if lateral_sway.empty:
        print("Warning: No data in lateral_sway.")
        return

    # Parameters for sway trail visualization
    trail_length = 50
    previous_lateral_displacements = []
    PLOT_INTERVAL = 15

    # Factors for sway calculation
    EXTEND_FACTOR = 1.5
    INTERPOLATE_FACTOR = 0.3

    # This threshold represents the minimum movement (in pixels) per frame to consider the animal as moving.
    MOVEMENT_THRESHOLD = 4

    previous_CoM_position = None

    # Get the total number of frames in the video
    total_frames = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))

    # Assert that the lengths of the lateral sway data and the total video frames are the same.
    assert len(lateral_sway) == total_frames, f"Mismatch in frame count: {len(lateral_sway)} sway measurements, {total_frames} video frames."

    # Initialize the 'movements' list with 'False' for each frame (indicating no movement initially)
    movements = [False] * total_frames

    # Before starting the processing loop, check the consistency of lateral_sway.
    if lateral_sway.empty or len(lateral_sway) != total_frames:
        print(f"Error: Data inconsistency detected. Lateral sway data contains {len(lateral_sway)} entries, expected {total_frames}.")
        return

    # Video processing loop
    while(cap.isOpened()):
        ret, frame = cap.read()
        if ret:
            # Calculating the body position and vectors
            body = np.array([CoM_x[frame_count], CoM_y[frame_count]])
            neck = np.array([cleaned_data.iloc[frame_count]['neck.x'], cleaned_data.iloc[frame_count]['neck.y']])
            tailbase = np.array([cleaned_data.iloc[frame_count]['tailbase.x'], cleaned_data.iloc[frame_count]['tailbase.y']])

            if not np.isnan(neck).any() and not np.isnan(tailbase).any() and not np.isnan(body).any():
                long_axis = tailbase - neck
                perpendicular = np.array([-long_axis[1], long_axis[0]])
                perpendicular_normalized = perpendicular / np.linalg.norm(perpendicular)

                lateral_displacement = np.dot(body - neck, perpendicular_normalized) * EXTEND_FACTOR

                if previous_lateral_displacements:
                    # Interpolation for smoother transition in displacements
                    lateral_displacement = (lateral_displacement * INTERPOLATE_FACTOR) + \
                                           (previous_lateral_displacements[-1] * (1 - INTERPOLATE_FACTOR))

                previous_lateral_displacements.append(lateral_displacement)

                if len(previous_lateral_displacements) > trail_length:
                    previous_lateral_displacements.pop(0)

                # Drawing the trail and the center of mass (red dot)
                for idx, displacement in enumerate(previous_lateral_displacements):
                    adjusted_point = body + displacement * perpendicular_normalized
                    adjusted_point = (int(adjusted_point[0]), int(adjusted_point[1]))
                    current_thickness = max(1, min(25, int(25 * (idx + 1) / len(previous_lateral_displacements))))
                    frame = cv2.line(frame, (int(body[0]), int(body[1])), adjusted_point, (255, 255, 255), thickness=current_thickness)

                frame = cv2.circle(frame, (int(body[0]), int(body[1])), radius=15, color=(0, 0, 255), thickness=-1)

            # Calculate the Center of Mass (CoM) position for the current frame.
            current_CoM_position = np.array([CoM_x[frame_count], CoM_y[frame_count]])

            # Determine if the animal is moving or standing based on the movement threshold.
            if previous_CoM_position is not None:
                movement = np.linalg.norm(current_CoM_position - previous_CoM_position)
                is_moving = movement >= MOVEMENT_THRESHOLD
            else:
                is_moving = False

            # Update the 'movements' list for the current frame
            movements[frame_count] = is_moving
            previous_CoM_position = current_CoM_position

            # This is inside your while loop where you process the video frames
            if frame_count % PLOT_INTERVAL == 0:
                fig, ax = plt.subplots(figsize=(11, 8))

            # Update the call to include the 'video' style
                ax = update_lateral_sway_plot(ax, lateral_sway, movements, frame_count, annotation_style="video")


                # Saving the plot to a buffer
                buf = BytesIO()
                plt.savefig(buf, format='png', dpi=100)
                buf.seek(0)
                lateral_sway_plot_img = cv2.imdecode(np.frombuffer(buf.read(), np.uint8), 1)

                # Resizing to match the video frame and concatenating with the video
                resized_plot = cv2.resize(lateral_sway_plot_img, (video_width, video_height))
                concatenated = np.hstack((frame, resized_plot))
                out.write(concatenated)

                # Closing the plot to free memory
                plt.close(fig)

            frame_count += 1
        else:
            break

    with PdfPages(plot_pdf_path_combined) as pdf:
        fig = plt.figure(figsize=(8.27, 11.69))  # A4 portrait dimensions
        gs = GridSpec(3, 1, figure=fig)  # 3 rows, 1 column

    # Lateral sway plot with red and blue lines
        lateral_sway_ax = fig.add_subplot(gs[0, 0])

    # Update the call to include the 'pdf' style
        update_lateral_sway_plot(lateral_sway_ax, lateral_sway, movements, last_frame=frame_count, annotation_style="pdf")

        plt.subplots_adjust(top=0.85, bottom=0.15, left=0.10, right=0.95, hspace=0.4, wspace=0.35)
        pdf.savefig(fig)
        plt.close(fig)

    # Release video capture and writer resources
    cap.release()
    out.release()
    cv2.destroyAllWindows()

    # Now, you're returning the 'lateral_sway' and 'movements' for further usage outside this function
    return lateral_sway, movements


def match_csv_to_videos(csv_files, video_files):
    matched_files = []
    for csv_file in csv_files:
        pattern = re.search(r'(control|mutant)_?\s*(\d+)', csv_file, re.IGNORECASE)
        if pattern:
            group, number = pattern.groups()
            # Create a regex pattern for the video file based on the CSV file
            video_pattern = re.compile(rf'\b{group}_?\s*{number}\b', re.IGNORECASE)

            # Find the corresponding video file
            for video_file in video_files:
                if video_pattern.search(video_file):
                    matched_files.append((csv_file, video_file))
                    break
    return matched_files

def calculate_sem(data):
    """
    Calculate the Standard Error of the Mean (SEM) for a list of numbers.

    :param data: List of numbers
    :return: SEM value
    """
    if len(data) <= 1:
        return None  # Cannot calculate SEM with a single data point
    standard_deviation = np.std(data, ddof=1)  # ddof=1 indicates use of sample standard deviation
    sem = standard_deviation / np.sqrt(len(data))
    return sem

def calculate_sway_means(lateral_sway, movements):
    # standing_sway = [lateral_sway[i] for i, is_moving in enumerate(movements) if not is_moving]
    moving_sway = [lateral_sway[i] for i, is_moving in enumerate(movements) if is_moving]

    sway_moving_mean = np.mean(moving_sway) if moving_sway else None

    # return sway_standing_mean, sway_moving_mean
    return sway_moving_mean

def calculate_std_dev(data):
    """
    Calculate the Standard Deviation (SD) for a list of numbers.

    :param data: List of numbers
    :return: SD value
    """
    if len(data) <= 1:
        return None  # Cannot calculate SD with a single data point
    return np.std(data, ddof=1)  # ddof=1 indicates use of sample standard deviation

def analyze_and_plot_group_differences(control_means, mutant_means, category, pdf):
    """
    [Previous docstring here]
    """
    # Remove None values from the lists if present
    control_means = [x for x in control_means if x is not None]
    mutant_means = [x for x in mutant_means if x is not None]

    # Log the data for debugging
    logging.info(f"Control means for {category}: {control_means}")
    logging.info(f"Mutant means for {category}: {mutant_means}")

    # Check if the lists are empty or have insufficient data
    if not control_means or not mutant_means:
        logging.warning(f"No valid data for {category}. Skipping analysis.")
        print(f"No valid data for {category}. Skipping analysis.")
        return

    if len(control_means) < 2 or len(mutant_means) < 2:
        logging.warning(f"Not enough data points for {category}. Skipping t-test and plotting.")
        print(f"Not enough data points for {category}. Skipping t-test and plotting.")
        return

    # Calculate group statistics
    control_avg = np.mean(control_means)
    mutant_avg = np.mean(mutant_means)

    # Calculate the standard error of the mean for both groups
    control_sem = calculate_sem(control_means)
    mutant_sem = calculate_sem(mutant_means)

    try:
        # Perform the t-test
        t_stat, p_value = ttest_ind(control_means, mutant_means)

        # Create a bar plot with the results
        fig, ax = plt.subplots()
        categories = ['Control', 'Mutant']
        averages = [control_avg, mutant_avg]
        errors = [control_sem, mutant_sem]  # standard errors

        # Plotting the bars with error bars
        ax.bar(categories, averages, color=['blue', 'red'], alpha=0.7, yerr=errors, capsize=5)
        ax.set_ylabel('Average Lateral Sway')
        ax.set_title(f'Comparison of {category.capitalize()} Sway: p={p_value:.3e}')

        # Add text on the bars
        for i, v in enumerate(averages):
            ax.text(i, v + (0.02 if errors[i] is None else errors[i]), f"{v:.2f}", color='black', ha='center')

        # Save the plot to the PDF
        pdf.savefig(fig)
        plt.close(fig)
    except Exception as e:
        # If there's an error during the statistical test or plotting, log the exception
        logging.error(f"An error occurred during analysis of {category}: {e}", exc_info=True)
        print(f"An error occurred during analysis of {category}: {e}")

def create_pdf_table(data, filename):
    """
    Create a simple PDF table using fpdf.

    :param data: DataFrame containing the data
    :param filename: Path where the PDF will be saved
    """
    pdf = FPDF()
    pdf.add_page()

    # Add a title
    pdf.set_font("Arial", size=12)
    pdf.cell(200, 10, txt="Lateral Sway Analysis Report", ln=True, align='C')

    # Create a header
    col_width = pdf.w / len(data.columns)  # Divide the width of the page by the number of columns
    row_height = pdf.font_size * 1.5
    for column in data.columns:
        pdf.cell(col_width, row_height, txt=column, border=1)

    pdf.ln(row_height)  # Move to the next line

    # Add data rows
    for row in data.itertuples(index=False):
        for item in row:
            # Format the data to have two decimal places if it's a number
            if isinstance(item, (int, float)):
                text = f"{item:.2f}"
            else:
                text = str(item)
            pdf.cell(col_width, row_height, txt=text, border=1)
        pdf.ln(row_height)

    pdf.output(filename)

def main():
    # Setup logging to capture any errors during processing
    logging.basicConfig(filename='processing_errors.log', level=logging.ERROR)

    # Define the directory containing your files
    directory_path = 'www/uploads'  # Modify with your actual path

    # Retrieve all CSV and video files in the directory
    all_files = os.listdir(directory_path)
    csv_files = [os.path.join(directory_path, file) for file in all_files if file.endswith('.csv')]
    video_files = [os.path.join(directory_path, file) for file in all_files if file.endswith('.mov')]  # Assuming the videos are .mov files

    # Match CSVs to their corresponding videos
    matched_files = match_csv_to_videos(csv_files, video_files)

    # Specify the directory where the PDFs will be saved
    pdf_output_directory = '/Users/isaac/Desktop/test'  # This should be the directory, not a specific file

    # Initialize storage for individual animal data and counters for animal types
    all_animals_data = []
    animal_counters = {"control": 0, "mutant": 0}

    for csv_file, video_file in matched_files:
        try:
            # Check if files exist before processing
            if not os.path.exists(csv_file) or not os.path.exists(video_file):
                raise FileNotFoundError(f"One or both of the files {csv_file}, {video_file} do not exist.")

            # Load and clean the data from the CSV file
            data = load_csv_r_style(csv_file)
            cleaned_data = clean_tracking_data(data)

            # Derive a base name for output files based on the CSV file name
            base_name = os.path.splitext(os.path.basename(csv_file))[0]

            # Define the output paths for the processed video and the plot PDFs
            output_video_path = os.path.join(directory_path, f"{base_name}_processed.mov")
            plot_pdf_path_combined = os.path.join(pdf_output_directory, f"{base_name}_plots_combined.pdf")

            # Process the video and generate the combined PDF report
            lateral_sway, movements = concatenate_video_and_CoM_sway_from_df(video_file, cleaned_data, output_video_path, plot_pdf_path_combined)

            # Calculate sway means
            sway_moving_mean = calculate_sway_means(lateral_sway, movements)


            # Extract the type from the file name (control or mutant)
            animal_type = "control" if "control" in csv_file.lower() else "mutant"

            # Update the animal type counter and generate the unique identifier
            animal_counters[animal_type] += 1
            animal_id = f"{animal_type.capitalize()} {animal_counters[animal_type]}"

            # Calculate standard deviations for individual animal data
            moving_sway = [lateral_sway[i] for i, is_moving in enumerate(movements) if is_moving]

            # sway_standing_std = calculate_std_dev(standing_sway) if standing_sway else None
            sway_moving_std = calculate_std_dev(moving_sway) if moving_sway else None

            # Store individual animal data WITHOUT the 'Animal ID' column
            animal_data = {
                'Animal Type': animal_id,  # Using the unique identifier instead of just the type
                'Walking Mean': sway_moving_mean,
                'Walking SD': sway_moving_std
            }
            all_animals_data.append(animal_data)

        except Exception as e:
            error_message = f"An error occurred while processing {csv_file} and {video_file}: {str(e)}"
            print(error_message)  # This is for immediate console output, potentially redirect to stderr if preferred
            logging.error(error_message, exc_info=True)  # This logs the stack trace
            continue  # Continue with the next iteration

    # Create a DataFrame from the individual animal data
    df_results = pd.DataFrame(all_animals_data)

    # Create a new column 'SortKey' to sort the data without affecting the 'Animal Type' column
    df_results['SortKey'] = df_results['Animal Type'].str.extract(r'(\d+)', expand=False).astype(int)  # Extract number part
    df_results['SortCategory'] = df_results['Animal Type'].apply(lambda x: x.split(' ')[0])  # Extract category part

    # Define a custom sorting order for the 'SortCategory' column to ensure 'Control' comes before 'Mutant'
    sorting_order = pd.CategoricalDtype(['Control', 'Mutant'], ordered=True)
    df_results['SortCategory'] = df_results['SortCategory'].astype(sorting_order)

    # Now, sort the values first by 'SortCategory' and then by 'SortKey'.
    df_results = df_results.sort_values(by=['SortCategory', 'SortKey'])

    # Drop the sorting columns as they're no longer needed for the final presentation
    df_results = df_results.drop(columns=['SortKey', 'SortCategory'])

    # Create a PDF table with the results
    pdf_table_filename = os.path.join(pdf_output_directory, 'sway_analysis_table.pdf')
    create_pdf_table(df_results, pdf_table_filename)

    # Save the results in an Excel file
    excel_filename = os.path.join(pdf_output_directory, 'sway_analysis.xlsx')
    df_results.to_excel(excel_filename, index=False)

    print("PDF table and Excel file have been created.")

    # After all files have been processed, perform the group analysis
    with PdfPages(os.path.join(pdf_output_directory, 'group_analysis_plots.pdf')) as pdf:
        # Collect the data for group analysis based on the original 'Animal Type'
        control_group = df_results[df_results['Animal Type'].str.contains('Control')]
        mutant_group = df_results[df_results['Animal Type'].str.contains('Mutant')]

        for category in ['Walking']:  # Removed 'Standing' from the list
            control_means = control_group[f'{category} Mean'].dropna().tolist()
            mutant_means = mutant_group[f'{category} Mean'].dropna().tolist()

            analyze_and_plot_group_differences(control_means, mutant_means, category, pdf)

    print("Processing completed.")

# split into callable function
def run_full_analysis():
  main()
