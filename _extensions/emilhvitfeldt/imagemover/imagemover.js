window.RevealImagemover = function () {
  return {
    id: "RevealImagemover",
    init: function (deck) {
      document.addEventListener('DOMContentLoaded', function () {
        const imagemoverImages = document.querySelectorAll('img[id="imagemover"]');

        imagemoverImages.forEach(setupDraggableImage);

        // Find the slide-menu-items ul inside menu-custom-panel div
        const slideMenuItems = document.querySelector('div.slide-menu-custom-panel ul.slide-menu-items');

        if (slideMenuItems) {
          // Find the highest data-item value
          const existingItems = slideMenuItems.querySelectorAll('li[data-item]');
          let maxDataItem = 0;
          existingItems.forEach(item => {
            const dataValue = parseInt(item.getAttribute('data-item')) || 0;
            if (dataValue > maxDataItem) {
              maxDataItem = dataValue;
            }
          });

          // Create the new li element
          const newLi = document.createElement('li');
          newLi.className = 'slide-tool-item';
          newLi.setAttribute('data-item', (maxDataItem + 1).toString());
          newLi.innerHTML = '<a href="#" onclick="saveMovedImages()"><kbd>?</kbd> Save Moved Images</a>';

          // Append to the ul
          slideMenuItems.appendChild(newLi);
        }
      });
    },
  };
};

function setupDraggableImage(img) {
  let isDragging = false;
  let isResizing = false;
  let startX, startY, initialX, initialY, initialWidth, initialHeight;
  let resizeHandle = null;

  const container = createImageContainer(img);
  setupImageStyles(img);
  createResizeHandles(container);
  setupHoverEffects(container, () => isDragging, () => isResizing);
  attachEventListeners();

  function createImageContainer(img) {
    const container = document.createElement('div');
    container.style.position = 'absolute';
    container.style.display = 'inline-block';
    container.style.border = '2px solid transparent';
    img.parentNode.insertBefore(container, img);
    container.appendChild(img);
    return container;
  }

  function setupImageStyles(img) {
    img.style.cursor = 'move';
    img.style.position = 'relative';
    img.style.width = (img.naturalWidth || img.offsetWidth) / 2 + 'px';
    img.style.height = (img.naturalHeight || img.offsetHeight) / 2 + 'px';
    img.style.display = 'block';
  }

  function createResizeHandles(container) {
    const handles = ['nw', 'ne', 'sw', 'se'];
    handles.forEach(position => {
      const handle = document.createElement('div');
      handle.className = 'resize-handle';
      handle.style.position = 'absolute';
      handle.style.width = '10px';
      handle.style.height = '10px';
      handle.style.backgroundColor = '#007cba';
      handle.style.border = '1px solid #fff';
      handle.style.cursor = position + '-resize';
      handle.style.opacity = '0';
      handle.style.transition = 'opacity 0.2s';

      if (position.includes('n')) handle.style.top = '-6px';
      if (position.includes('s')) handle.style.bottom = '-6px';
      if (position.includes('w')) handle.style.left = '-6px';
      if (position.includes('e')) handle.style.right = '-6px';

      handle.dataset.position = position;
      container.appendChild(handle);
    });
  }

  function setupHoverEffects(container, isDraggingFn, isResizingFn) {
    container.addEventListener('mouseenter', () => {
      container.style.border = '2px solid #007cba';
      container.querySelectorAll('.resize-handle').forEach(h => h.style.opacity = '1');
    });

    container.addEventListener('mouseleave', () => {
      if (!isDraggingFn() && !isResizingFn()) {
        container.style.border = '2px solid transparent';
        container.querySelectorAll('.resize-handle').forEach(h => h.style.opacity = '0');
      }
    });
  }

  function attachEventListeners() {
    img.addEventListener('mousedown', startDrag);
    img.addEventListener('touchstart', startDrag);

    container.querySelectorAll('.resize-handle').forEach(handle => {
      handle.addEventListener('mousedown', startResize);
      handle.addEventListener('touchstart', startResize);
    });

    document.addEventListener('mousemove', handleMouseMove);
    document.addEventListener('mouseup', stopAction);
    document.addEventListener('touchmove', handleTouchMove);
    document.addEventListener('touchend', stopAction);
  }

  function getClientCoordinates(e) {
    const isTouch = e.type.startsWith('touch');

    // revealjs scales this .slides container element so that
    // the slide fits completely in the viewport. We have to
    // adjust the mouse/touch positions by this scaling.
    const slidesContainerEl = document.querySelector('.slides')
    const scale =  window.getComputedStyle(slidesContainerEl).getPropertyValue('--slide-scale')

    return {
      clientX: (isTouch ? e.touches[0].clientX : e.clientX)/scale,
      clientY: (isTouch ? e.touches[0].clientY : e.clientY)/scale
    };
  }

  function startDrag(e) {
    if (e.target.classList.contains('resize-handle')) return;

    isDragging = true;
    const { clientX, clientY } = getClientCoordinates(e);

    startX = clientX;
    startY = clientY;
    initialX = container.offsetLeft;
    initialY = container.offsetTop;

    e.preventDefault();
  }

  function startResize(e) {
    isResizing = true;
    resizeHandle = e.target.dataset.position;

    const { clientX, clientY } = getClientCoordinates(e);

    startX = clientX;
    startY = clientY;
    initialWidth = img.offsetWidth;
    initialHeight = img.offsetHeight;
    initialX = container.offsetLeft;
    initialY = container.offsetTop;

    e.preventDefault();
    e.stopPropagation();
  }

  function handleMouseMove(e) {
    if (isDragging) {
      drag(e);
    } else if (isResizing) {
      resize(e);
    }
  }

  function handleTouchMove(e) {
    if (isDragging) {
      drag(e);
    } else if (isResizing) {
      resize(e);
    }
  }

  function drag(e) {
    if (!isDragging) return;

    const { clientX, clientY } = getClientCoordinates(e);
    const deltaX = clientX - startX;
    const deltaY = clientY - startY;

    container.style.left = (initialX + deltaX) + 'px';
    container.style.top = (initialY + deltaY) + 'px';

    e.preventDefault();
  }

  function resize(e) {
    if (!isResizing) return;

    const { clientX, clientY } = getClientCoordinates(e);
    const deltaX = clientX - startX;
    const deltaY = clientY - startY;

    let newWidth = initialWidth;
    let newHeight = initialHeight;
    let newX = initialX;
    let newY = initialY;

    // Check if Shift key is pressed for aspect ratio preservation
    const preserveAspectRatio = e.shiftKey;
    const aspectRatio = initialWidth / initialHeight;

    if (preserveAspectRatio) {
      // For corner handles, use the larger delta to maintain aspect ratio
      if (resizeHandle.includes('e') || resizeHandle.includes('w')) {
        const widthChange = resizeHandle.includes('e') ? deltaX : -deltaX;
        newWidth = Math.max(50, initialWidth + widthChange);
        newHeight = newWidth / aspectRatio;
      } else if (resizeHandle.includes('s') || resizeHandle.includes('n')) {
        const heightChange = resizeHandle.includes('s') ? deltaY : -deltaY;
        newHeight = Math.max(50, initialHeight + heightChange);
        newWidth = newHeight * aspectRatio;
      }

      // Adjust position for west/north handles when preserving aspect ratio
      if (resizeHandle.includes('w')) {
        newX = initialX + (initialWidth - newWidth);
      }
      if (resizeHandle.includes('n')) {
        newY = initialY + (initialHeight - newHeight);
      }
    } else {
      // Original free resize behavior
      if (resizeHandle.includes('e')) {
        newWidth = Math.max(50, initialWidth + deltaX);
      }
      if (resizeHandle.includes('w')) {
        newWidth = Math.max(50, initialWidth - deltaX);
        newX = initialX + (initialWidth - newWidth);
      }
      if (resizeHandle.includes('s')) {
        newHeight = Math.max(50, initialHeight + deltaY);
      }
      if (resizeHandle.includes('n')) {
        newHeight = Math.max(50, initialHeight - deltaY);
        newY = initialY + (initialHeight - newHeight);
      }
    }

    img.style.width = newWidth + 'px';
    img.style.height = newHeight + 'px';
    container.style.left = newX + 'px';
    container.style.top = newY + 'px';

    e.preventDefault();
  }

  function stopAction() {
    if (isDragging || isResizing) {
      setTimeout(() => {
        if (!container.matches(':hover')) {
          container.style.border = '2px solid transparent';
          container.querySelectorAll('.resize-handle').forEach(h => h.style.opacity = '0');
        }
      }, 500);
    }

    isDragging = false;
    isResizing = false;
    resizeHandle = null;
  }
}

async function saveMovedImages() {
  index = await readIndexQmd()
  image_dim = extractimagemoverImageDimensions()
  image_attr = formatimagemoverImageStrings(image_dim)
  index = replaceimagemoverOccurrences(index, image_attr)
  downloadString(index)
}
// Function to read index.qmd file
async function readIndexQmd() {
  try {
    const response = await fetch(getImageMoverFilename());
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    const content = await response.text();
    return content;
  } catch (error) {
    console.error('Error reading index.qmd:', error);
    return null;
  }
}

// Function to get data-filename attribute from imagemover div
function getImageMoverFilename() {
  const imageMoverDiv = document.getElementById('filename');
  return imageMoverDiv ? imageMoverDiv.getAttribute('data-filename') : null;
}

// Function to extract width and height of images with imagemover id
function extractimagemoverImageDimensions() {
  const imagemoverImages = document.querySelectorAll('img[id="imagemover"]');
  const dimensions = [];

  imagemoverImages.forEach((img, index) => {
    const width = img.style.width ? parseFloat(img.style.width) : img.offsetWidth;
    const height = img.style.height ? parseFloat(img.style.height) : img.offsetHeight;

    // Get parent container (div) position
    const parentContainer = img.parentNode;
    const left = parentContainer.style.left ? parseFloat(parentContainer.style.left) : parentContainer.offsetLeft;
    const top = parentContainer.style.top ? parseFloat(parentContainer.style.top) : parentContainer.offsetTop;

    dimensions.push({
      width: width,
      height: height,
      left: left,
      top: top
    });
  });

  return dimensions;
}

// Function to replace all occurrences that start with "{#imagemover" and go until the first "}" with replacements from array
function replaceimagemoverOccurrences(text, replacements) {
  const regex = /\{#imagemover[^}]*\}/g;
  let index = 0;
  return text.replace(regex, () => {
    return replacements[index++] || '';
  });
}

// Function to format imagemover image dimensions as strings
function formatimagemoverImageStrings(dimensions) {
  return dimensions.map(dim => {
    return `{.absolute width=${dim.width}px height=${dim.height}px left=${dim.left}px top=${dim.top}px}`;
  });
}

// Function to make a string available as a downloadable file
async function downloadString(content, mimeType = 'text/plain') {
  filename = getImageMoverFilename();
  // Check if the File System Access API is supported
  if ('showSaveFilePicker' in window) {
    try {
      // Show file picker dialog
      const fileHandle = await window.showSaveFilePicker({
        suggestedName: filename,
        types: [{
          description: 'Text files',
          accept: { [mimeType]: ['.txt', '.qmd', '.md'] }
        }]
      });

      // Create a writable stream and write the content
      const writable = await fileHandle.createWritable();
      await writable.write(content);
      await writable.close();

      console.log('File saved successfully');
      return;
    } catch (error) {
      // User cancelled or error occurred, fall back to traditional method
      console.log('File picker cancelled or failed, using fallback method');
    }
  }

  // Fallback to traditional download method
  const blob = new Blob([content], { type: mimeType });
  const url = URL.createObjectURL(blob);

  const a = document.createElement('a');
  a.href = url;
  a.download = filename;

  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);

  URL.revokeObjectURL(url);
}
